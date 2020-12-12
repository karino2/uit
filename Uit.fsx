open System

type Hash = Hash of byte array

type UPath = UPath of string

// スラッシュ無しで始まりスラッシュ無しで終わる。
// rootは""で表す。
type UDir = UDir of UPath
type OSPath = OSPath of string

// このPathは最後のスラッシュは含まない
type Repo = { Path: OSPath }

type PathEntry = {
    Path : UPath
    LastModified : DateTime
    EntryDate: DateTime
}

type TypedPathEntry =
| Instance of PathEntry
| Reference of PathEntry


// .uit/hash/xx/yyyyyyyyyyy.txt にかかれている情報
// hashはパスから取り出す。
// 各行はそのハッシュのblobに対応したパスの情報を表す。
// 一行は、
// t LastModified EntryDate   UPath
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


type FInfo = {
    Hash : Hash
    MetaInfo : TypedPathEntry
}

type ComputeHash = Repo-> UPath -> Hash
type ToBlobInfo = Repo -> Hash -> ManagedFile option
type PathToBlobInfo = Repo -> UPath -> BlobInfo

type ImportOne = Repo -> OSPath -> UPath -> ManagedFile
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
// LastModified EntryDate Hash ファイル名
// となっている。
type DirInfo = (Repo * UDir) -> FInfo list
type ToFInfo = (Repo * UPath) -> FInfo option


type FInfosToText = FInfo list -> string
type ManagedFileToText = ManagedFile -> string

type SaveText = (OSPath * string) -> unit



open System.IO
open System.Text
open System.Security.Cryptography

let sha = SHA256.Create()

let computeRawFileHash (sha:SHA256) (path:string) =
    use reader = new FileStream(path, FileMode.Open)
    sha.ComputeHash reader

let toOSPath (repo:Repo) upath =
    let (OSPath bpath) = repo.Path
    let (UPath value) = upath
    if value = "" then
        OSPath (bpath + "/")
    else
        OSPath (Path.Combine(bpath, value))

let computeFileHash ospath =
    let (OSPath bpath) = ospath
    Hash (computeRawFileHash sha bpath)

let bytesToString (bytes: byte[])=
    bytes |> Array.map (sprintf "%x") |> String.concat ""

let hashDir hash =
    let (Hash bytes) = hash
    (UPath (sprintf ".uit/hash/%x/" bytes.[0]))


let hashPath hash =
    let (Hash bytes) = hash
    let (UPath dir) = hashDir hash
    (UPath (sprintf "%s%s.txt" dir (bytesToString bytes.[1..])))


let fileinfo ospath =
    let (OSPath value) = ospath
    FileInfo value


let mfToText mf =
    let totext tp (pe: PathEntry) =
        let (UPath path) = pe.Path
        sprintf "%d\t%d\t%d\t%s\n"  tp pe.LastModified.Ticks pe.EntryDate.Ticks path
    let instances = mf.InstancePathList |> List.map (totext 1)
    let refs = mf.ReferencePathList |> List.map (totext 2)
    List.append instances refs |> List.reduce (+)


let ensureDir (di: DirectoryInfo) =
    if not di.Exists then
        Directory.CreateDirectory di.FullName |> ignore
    

let rawSaveTo (fi: FileInfo) text =
    ensureDir fi.Directory
    File.WriteAllText("temp.dat", text)
    File.Move("temp.dat", fi.FullName)


let saveText ospath text =
    let (OSPath path) = ospath
    rawSaveTo (FileInfo path) text

let toRawFileInfo repo upath =
    let (OSPath path) =  toOSPath repo upath
    FileInfo path

let toBlobInfo repo hash =
    let dir = hashPath hash
    let fi = toRawFileInfo repo dir
    let toPE (cells : string array) =
        {Path=(UPath cells.[2]); LastModified=DateTime(Int64.Parse cells.[0]); EntryDate=DateTime(Int64.Parse cells.[1])}
    let toIoR (line: string) =
        let cells = line.Split('\t')
        let tp = cells.[0]
        match tp with
        | "1" -> Instance (toPE cells.[1..])
        | "2" -> Reference (toPE cells.[1..])
        | _ -> failwith "invalid data"
    if fi.Exists then
        let onlyIorR icase rcase =
            fun m ->
                match m with
                |Instance _-> icase
                |Reference _ -> rcase
        let inside ior =
            match ior with
            | Instance entry -> entry
            | Reference entry -> entry

        let ret = 
            File.ReadLines fi.FullName
            |> Seq.map toIoR

        let is = ret |> Seq.filter (onlyIorR true false) |> Seq.map inside |> Seq.toList
        let rs = ret |> Seq.filter (onlyIorR false true) |> Seq.map inside |> Seq.toList
        ManagedFile { Hash = hash; InstancePathList=is; ReferencePathList=rs }
    else
        UnmanagedFile


let repo = { Path = OSPath "/Users/arinokazuma/work/testdata" }

let h = computeFileHash (OSPath "/Users/arinokazuma/work/testdata/Uit.fsx")

let fi = FileInfo("/Users/arinokazuma/work/testdata/Uit.fsx")
let entryCreated = DateTime.Now

let pe = {Path=(UPath "Uit.fsx"); LastModified=fi.LastWriteTime; EntryDate=entryCreated }
let mf = {Hash= h; InstancePathList=[pe]; ReferencePathList=[]}

mfToText mf

let dest = hashPath mf.Hash
saveText (toOSPath repo dest) (mfToText mf)

toBlobInfo repo h


let toUPath (repo:Repo) ospath =
    let (OSPath from) = ospath
    let (OSPath repopath) = repo.Path
    if not (from.StartsWith repopath) then
        failwith "ospath is not under repo"
    UPath (from.Substring repopath.Length)

let toDirInfo repo udir =
    let (UDir path) = udir
    let (OSPath abspath) = toOSPath repo path
    DirectoryInfo abspath

let createUDir repo (abspath:string) =    
    let upath = toUPath repo (OSPath (abspath.TrimEnd '/'))
    UDir (upath)

let root = createUDir repo "/Users/arinokazuma/work/testdata/"

let rootDI = toDirInfo repo root

rootDI.EnumerateFiles()
|> Seq.iter (fun fi-> printfn "%s" fi.Name )

rootDI.EnumerateFiles()
|> Seq.map (fun fi-> OSPath fi.FullName)


let dir = DirectoryInfo( "/Users/arinokazuma/work/testdata/" )

dir.EnumerateFiles()
|> Seq.iter (fun fi-> printfn "%s" fi.Name )



repo


"/hoge/ika/".TrimEnd('/')


toUPath repo (OSPath "/Users/arinokazuma/work/testdata/")

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
mfToText mf

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
bytesToString h.[1..]


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