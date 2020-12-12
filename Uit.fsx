open System
open System.IO

type Hash = Hash of byte array

type UPath = UPath of string

// スラッシュ無しで始まりスラッシュ無しで終わる。
// rootは""で表す。
type UDir = UDir of UPath

// このPathは最後のスラッシュは含まない
type Repo = { Path: DirectoryInfo }

type PathEntry = {
    Path : UPath
    LastModified : DateTime
    EntryDate: DateTime
}

type TypedPathEntry =
| InstancePE of PathEntry
| ReferencePE of PathEntry


// .uit/hash/xx/yyyyyyyyyyy.txt にかかれている情報
// hashはパスから取り出す。
// 各行はそのハッシュのblobに対応したパスの情報を表す。
// 一行は、
// t LastModified EntryDate   fname
// の形式で、LastModifiedは対応するblobをimportした時のfileのlastmodified。
// EntryDateはこの行を更新した時の日時
// tはinstanceなら1、referenceなら2。
type ManagedFile = { 
    Hash : Hash
    InstancePathList : PathEntry list
    ReferencePathList : PathEntry list
}

type BlobInfo = 
| ManagedFile of ManagedFile
| UnmanagedFile

type ComputeHash = (Repo * UPath) -> Hash
type ToBlobInfo = (Repo * Hash) -> ManagedFile option
type PathToBlobInfo = (Repo * UPath) -> BlobInfo

type ImportOne = (Repo * FileInfo * UPath) -> ManagedFile
type ListHash = Repo -> Hash list
type ListBlobInfo = Repo -> ManagedFile list

type SaveBlobInfo = (Repo * ManagedFile) -> unit

type InstancePaths = ManagedFile -> PathEntry list
type ReferencePaths = ManagedFile -> PathEntry list

type ToReference = (Repo * ManagedFile * UPath) -> ManagedFile
type ToInstance = (Repo * ManagedFile * UPath) -> ManagedFile
type UniqIt = Repo -> ManagedFile -> ManagedFile

type Paths = Repo -> Hash -> UPath list

// ".uit/dirs/"下にファイルのパスに対応した情報がある。
// dirsの下にディレクトリ構造そのままコピーしたディレクトリ構造があり、
// 目的のディレクトリエントリにはdir.txtというファイルがある。
// 例えば hoge/ika/test.txtというファイルの情報は、
// .uit/dirs/hoge/ika/dir.txt にある。
// このdir.txtの中身の各行はFInfoを表し、
// tp LastModified EntryDate Hash ファイル名
// となっている。tpはinstanceなら1, referenceなら2
// EntryDateはhash下と同じ物を使う。
type FInfoEntry = {
    Hash: Hash
    FName: string
    LastModified: DateTime
    EntryDate: DateTime
}
type FInfo = 
| InstanceFile of FInfoEntry
| ReferenceFile of FInfoEntry

type DirInfo = (Repo * UDir) -> FInfo list
type ToFInfo = (Repo * UPath) -> FInfo option


type FInfosToText = FInfo list -> string
type ManagedFileToText = ManagedFile -> string

type SaveText = (FileInfo * string) -> unit



open System.Text
open System.Security.Cryptography

let sha = SHA256.Create()

let computeRawFileHash (sha:SHA256) (path:string) =
    use reader = new FileStream(path, FileMode.Open)
    sha.ComputeHash reader


let toFileInfo (repo:Repo) upath =
    let (UPath value) = upath
    FileInfo(Path.Combine(repo.Path.FullName, value))

let computeFileHash (fi:FileInfo) =
    Hash (computeRawFileHash sha fi.FullName)

let bytes2string (bytes: byte[])=
    bytes |> Array.map (sprintf "%x") |> String.concat ""

let hash2string hash =
    let (Hash value) = hash
    bytes2string value

let hashDir hash =
    let (Hash bytes) = hash
    (UPath (sprintf ".uit/hash/%x/" bytes.[0]))


let hashPath hash =
    let (Hash bytes) = hash
    let (UPath dir) = hashDir hash
    (UPath (sprintf "%s%s.txt" dir (bytes2string bytes.[1..])))


let mf2text mf =
    let totext tp (pe: PathEntry) =
        let (UPath path) = pe.Path
        sprintf "%d\t%d\t%d\t%s\n"  tp pe.LastModified.Ticks pe.EntryDate.Ticks path
    let instances = mf.InstancePathList |> List.map (totext 1)
    let refs = mf.ReferencePathList |> List.map (totext 2)
    List.append instances refs |> List.reduce (+)


let ensureDir (di: DirectoryInfo) =
    if not di.Exists then
        Directory.CreateDirectory di.FullName |> ignore
    

let saveText (fi: FileInfo) text =
    ensureDir fi.Directory
    File.WriteAllText("temp.dat", text)
    File.Move("temp.dat", fi.FullName, true)


let toBlobInfo repo hash =
    let dir = hashPath hash
    let fi = toFileInfo repo dir
    let toPE (cells : string array) =
        {Path=(UPath cells.[2]); LastModified=DateTime(Int64.Parse cells.[0]); EntryDate=DateTime(Int64.Parse cells.[1])}
    let toIoR (line: string) =
        let cells = line.Split('\t')
        let tp = cells.[0]
        match tp with
        | "1" -> InstancePE (toPE cells.[1..])
        | "2" -> ReferencePE (toPE cells.[1..])
        | _ -> failwith "invalid data"
    if fi.Exists then
        let onlyIorR icase rcase =
            fun m ->
                match m with
                |InstancePE _-> icase
                |ReferencePE _ -> rcase
        let inside ior =
            match ior with
            | InstancePE entry -> entry
            | ReferencePE entry -> entry
        let ret = 
            File.ReadLines fi.FullName
            |> Seq.map toIoR
        let is = ret |> Seq.filter (onlyIorR true false) |> Seq.map inside |> Seq.toList
        let rs = ret |> Seq.filter (onlyIorR false true) |> Seq.map inside |> Seq.toList
        ManagedFile { Hash = hash; InstancePathList=is; ReferencePathList=rs }
    else
        UnmanagedFile

let diEquals (di1:DirectoryInfo) (di2:DirectoryInfo) =
    di1.FullName.TrimEnd(Path.DirectorySeparatorChar) = di2.FullName.TrimEnd(Path.DirectorySeparatorChar)

let toUPath (repo:Repo) (from:FileInfo) =
    let fromAbs = from.FullName
    let repoAbs = repo.Path.FullName + "/"
    if not (fromAbs.StartsWith repoAbs) then
        failwith "from is not under repo"
    UPath (fromAbs.Substring repoAbs.Length)

let toUDir (repo:Repo) (from:DirectoryInfo) =
    let relative =
        if diEquals repo.Path from then
            ""
        else
            let fromAbs = from.FullName
            let repoAbs = repo.Path.FullName + "/"
            if not (fromAbs.StartsWith repoAbs) then
                failwith "from is not under repo"
            fromAbs.Substring repoAbs.Length
    relative
    |> UPath |> UDir

let toDirInfo repo udir =
    let (UDir path) = udir
    let fi = toFileInfo repo path
    fi.FullName.TrimEnd(Path.DirectorySeparatorChar)
    |> DirectoryInfo

let createUDir repo (abspath:string) =    
    toUDir repo (DirectoryInfo (abspath.TrimEnd Path.DirectorySeparatorChar))

let computeFInfo fi =
    let hash = computeFileHash fi
    InstanceFile {Hash = hash; FName=fi.Name; LastModified=fi.LastWriteTime; EntryDate=DateTime.Now}

let finfo2text finfo =
    let fmt tp (lastmod:DateTime) (entdt:DateTime) hash fname =
        sprintf "%d\t%d\t%d\t%s\t%s" tp lastmod.Ticks entdt.Ticks (hash2string hash) fname
    let format tp pe =
        fmt tp pe.LastModified pe.EntryDate pe.Hash pe.FName
    match finfo with
    | InstanceFile pe -> format 1 pe
    | ReferenceFile pe -> format 2 pe

let dirFileFI (repo:Repo) udir =
    let dirRoot = Path.Combine(repo.Path.FullName, ".uit", "dirs")
    let (UDir (UPath relative)) = udir
    let dir = 
        if String.IsNullOrEmpty relative then
            dirRoot
        else
            Path.Combine(dirRoot, relative)
    Path.Combine(dir, "dir.txt") |> FileInfo




let repo = { Path = DirectoryInfo "/Users/arinokazuma/work/testdata" }

let h = computeFileHash (FileInfo "/Users/arinokazuma/work/testdata/Uit.fsx")

let fi = FileInfo("/Users/arinokazuma/work/testdata/Uit.fsx")
let entryCreated = DateTime.Now

let pe = {Path=(UPath "Uit.fsx"); LastModified=fi.LastWriteTime; EntryDate=entryCreated }
let mf = {Hash= h; InstancePathList=[pe]; ReferencePathList=[]}

mf2text mf

let dest = hashPath mf.Hash
saveText (toFileInfo repo dest) (mf2text mf)

toBlobInfo repo h


let upath = toUPath repo (FileInfo "/Users/arinokazuma/work/testdata/Uit.fsx")

let finfo = computeFInfo (FileInfo "/Users/arinokazuma/work/testdata/Uit.fsx")






let root = createUDir repo "/Users/arinokazuma/work/testdata/"

let rootDI = toDirInfo repo root

let finfos = rootDI.EnumerateFiles()
             |> Seq.map computeFInfo
             |> Seq.toList

finfos
|> List.map finfo2text
|> String.concat "\n"


finfo2text finfo

let dirfi = dirFileFI repo root
dirfi.Directory |> ensureDir

finfos
|> List.map finfo2text
|> String.concat "\n"
|> saveText dirfi

File.ReadLines dirfi.FullName
|> Seq.iter (fun line-> printfn "%s" line)


finfo2text finfo
|> saveText dirfi


let dir = DirectoryInfo( "/Users/arinokazuma/work/testdata/" )

dir.EnumerateFiles()
|> Seq.iter (fun fi-> printfn "%s" fi.Name )


let toFName upath =
    let (UPath path) = upath
    path.Split("/")
    |> Array.last



repo


"/hoge/ika/".TrimEnd('/')


let dirInfo1 = DirectoryInfo "/Users/arinokazuma/work/testdata"
let dirInfo2 = DirectoryInfo "/Users/arinokazuma/work/"



diEquals dirInfo1.Parent dirInfo2

dirInfo1.Parent.FullName.TrimEnd(Path.DirectorySeparatorChar) = dirInfo2.FullName.TrimEnd(Path.DirectorySeparatorChar)

dirInfo1.Parent.FullName

dirInfo2.FullName

dirInfo1.Parent = dirInfo2
dirInfo1.Parent.Equals dirInfo2

(DirectoryInfo "../").FullName



let from = "/Users/arinokazuma/work/testdata/"
let repopath = "/Users/arinokazuma/work/testdata"

from.StartsWith(repopath)

(*
DirectoryInfo dirPrograms = new DirectoryInfo(docPath);
            DateTime StartOf2009 = new DateTime(2009, 01, 01);

            var dirs = from dir in dirPrograms.EnumerateDirectories(
*)


fi.LastWriteTime

DateTime(637433260866558117L)

(UPath "Uit.fsx")






(*
> toBlobInfo repo h;;
val it : BlobInfo =
  ManagedFile
    { Hash =
            Hash
              [|254uy; 80uy; 119uy; 45uy; 58uy; 155uy; 121uy; 6uy; 223uy; 27uy;
                167uy; 189uy; 75uy; 223uy; 52uy; 220uy; 98uy; 114uy; 214uy;
                109uy; 91uy; 219uy; 217uy; 16uy; 152uy; 153uy; 1uy; 225uy;
                60uy; 51uy; 26uy; 36uy|]
      InstancePathList =
                        [{ Path = UPath "Uit.fsx"
                           LastModified =
                                         2020/12/11 23:28:06
                                           {Date = 2020/12/11 0:00:00;
                                            Day = 11;
                                            DayOfWeek = Friday;
                                            DayOfYear = 346;
                                            Hour = 23;
                                            Kind = Unspecified;
                                            Millisecond = 655;
                                            Minute = 28;
                                            Month = 12;
                                            Second = 6;
                                            Ticks = 637433260866558117L;
                                            TimeOfDay = 23:28:06.6558117;
                                            Year = 2020;}
                           EntryDate =
                                      2020/12/11 23:34:30
                                        {Date = 2020/12/11 0:00:00;
                                         Day = 11;
                                         DayOfWeek = Friday;
                                         DayOfYear = 346;
                                         Hour = 23;
                                         Kind = Unspecified;
                                         Millisecond = 499;
                                         Minute = 34;
                                         Month = 12;
                                         Second = 30;
                                         Ticks = 637433264704990800L;
                                         TimeOfDay = 23:34:30.4990800;
                                         Year = 2020;} }]
      ReferencePathList = [] }*)

let f = FileInfo("hoge/ika/fuga")
f.Directory
f.DirectoryName
mf2text mf

Int64.Parse("637433260866558117")

["a"; "b"; "c"] |> String.concat "/"

List.append [1; 2; 3] [4; 5; 6]

h

hashPath h



// let pathToFInfo repo upath =

// let toFInfo repo upath =

// let importOne repo (src: OSPath) (dest: UPath) =




let exists repo upath =
    let fi = toHash repo upath |> hashPath |> toOSPath repo |> fileinfo
    fi.Exists

let repo = { Path = OSPath "/Users/arinokazuma/work/uit" }

let h = osPathToHash (OSPath "/Users/arinokazuma/work/uit/usecase.fsx")

hashPath h


let fi = fileinfo (OSPath "/Users/arinokazuma/work/uit/usecase.fsx")
fi.Exists

let fi = fileinfo (OSPath "/Users/arinokazuma/work/uit/.uit/01/23456789.txt")
fi.Exists

let fi = fileinfo (OSPath "/Users/arinokazuma/work/uit/.uit/01/")
Directory.CreateDirectory fi.FullName
fi.FullName
fi.Create()

fi.Directory

sprintf "%x" h.[0]
bytes2string h.[1..]


let upath path =
    let canonical =
        if path = "./" then
            ""
        elif path.[0] = '/' then
            path.[1..]
        else
            path
    UPath canonical





let hash = sha.ComputeHash (Encoding.UTF8.GetBytes "Hello World")

hash.Length

let createReader path = new System.IO.StreamReader(path = path)





computeHash sha "/Users/arinokazuma/work/uit/usecase.fsx"





let toOSPath repo path =
    let (OSPath bpath) = repo.Path
    let (UPath value) = path
    if value = "" then
        bpath + "/"
    else
        Path.Combine(bpath, value)


let p = upath "/ika"

toOSPath repo p
toOSPath repo (upath "./")

Directory.GetFiles(toOSPath repo (upath "./"))

let dir repo path =
    let ospath = toOSPath repo path
    Directory.GetFiles ospath


dir repo (upath "./")

Path.Combine("hoge", "/ika")


// let dir repo path =


(*
let uit1 = UitRepo "/User/local/karino2/disk1/uit1/"
let uit2 = UitRepo "/User/local/karino2/disk2/uit2/"

let uit1_2 = add "some/relative/file.txt" uit1
*)