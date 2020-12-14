open System
open System.IO

type Hash = Hash of byte array

type UPath = UPath of string

// スラッシュ無しで始まりスラッシュ無しで終わる。
// rootは""で表す。
type UDir = UDir of UPath

// このPathは最後のスラッシュは含まない
type Repo = { Path: DirectoryInfo }


type FileType =
| Instance
| Reference

type PathEntry = {
    Type: FileType
    Path : UPath
    LastModified : DateTime
    EntryDate: DateTime
}

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

type ComputeHash = Repo -> UPath -> Hash
type ToBInfo = Repo -> Hash -> BlobInfo
type PathToBInfo = Repo -> UPath -> BlobInfo

type ImportOne = Repo -> FileInfo -> UPath -> ManagedFile

type ListHash = Repo -> Hash list
type ListMF = Repo -> ManagedFile list
type ListDupMF = Repo -> ManagedFile list

type SaveBInfo = Repo -> ManagedFile -> unit

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
type FInfo = {
    Type: FileType
    Hash: Hash
    FName: string
    LastModified: DateTime
    EntryDate: DateTime
}

// Repo下のファイルの.uit/hash と .uit/dirsを作る。
// まだhashなどが存在しない状態で行われるのでImportとは処理を分けておく
// .uit/dirsを作る都合でファイル単位じゃなくてディレクトリ単位
type InitOneDir = Repo -> UDir -> FInfo list

// Repo下のファイルを全てなめて.uit/hashと.uit/dirsを作る
type Init = Repo -> unit

type DirInfo = Repo -> UDir -> FInfo list
type ToFInfo = Repo -> UPath -> FInfo option
type UPath2BInfo  = Repo -> UPath -> BlobInfo option

type FInfos2Text = FInfo list -> string
type ManagedFileToText = ManagedFile -> string

type SaveText = FileInfo -> string -> unit

//
// Implementation
// 

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
    bytes |> Array.map (sprintf "%02x") |> String.concat ""

let hash2string hash =
    let (Hash value) = hash
    bytes2string value

let hashDir hash =
    let (Hash bytes) = hash
    (UPath (sprintf ".uit/hash/%02x/" bytes.[0]))


let hashPath hash =
    let (Hash bytes) = hash
    let (UPath dir) = hashDir hash
    (UPath (sprintf "%s%s.txt" dir (bytes2string bytes.[1..])))


let mf2text :ManagedFileToText = fun mf->
    let totext tp (pe: PathEntry) =
        let (UPath path) = pe.Path
        sprintf "%d\t%d\t%d\t%s\n"  tp pe.LastModified.Ticks pe.EntryDate.Ticks path
    let instances = mf.InstancePathList |> List.map (totext 1)
    let refs = mf.ReferencePathList |> List.map (totext 2)
    List.append instances refs |> List.reduce (+)


let ensureDir (di: DirectoryInfo) =
    if not di.Exists then
        Directory.CreateDirectory di.FullName |> ignore
    

let saveText :SaveText = fun fi text ->
    ensureDir fi.Directory
    File.WriteAllText("temp.dat", text)
    File.Move("temp.dat", fi.FullName, true)

let parseFileType str =
    match str with
    | "1" -> Instance
    | "2" -> Reference
    | _ -> failwith "invalid data"

let toBlobInfo :ToBInfo = fun repo hash ->
    let dir = hashPath hash
    let fi = toFileInfo repo dir
    let toIoR (line: string) =
        let cells = line.Split('\t', 4)
        let tp = parseFileType cells.[0]
        {Type = tp; Path=(UPath cells.[3]); LastModified=DateTime(Int64.Parse cells.[1]); EntryDate=DateTime(Int64.Parse cells.[2])}
    if fi.Exists then
        let onlyIorR icase rcase =
            fun (m:PathEntry) ->
                match m.Type with
                |Instance _-> icase
                |Reference _ -> rcase
        let ret = 
            File.ReadLines fi.FullName
            |> Seq.map toIoR
        let is = ret |> Seq.filter (onlyIorR true false) |> Seq.toList
        let rs = ret |> Seq.filter (onlyIorR false true) |> Seq.toList
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
    {Type=Instance; Hash = hash; FName=fi.Name; LastModified=fi.LastWriteTime; EntryDate=DateTime.Now}

let finfo2text finfo =
    let fmt tp (lastmod:DateTime) (entdt:DateTime) hash fname =
        sprintf "%d\t%d\t%d\t%s\t%s" tp lastmod.Ticks entdt.Ticks (hash2string hash) fname
    let format tp pe =
        fmt tp pe.LastModified pe.EntryDate pe.Hash pe.FName
    match finfo.Type with
    | Instance -> format 1 finfo
    | Reference -> format 2 finfo

let finfos2text finfos =
    finfos
    |> List.map finfo2text
    |> String.concat "\n"

let computeFInfoList repo udir =
    let di = toDirInfo repo udir
    di.EnumerateFiles()
    |> Seq.map computeFInfo
    |> Seq.toList



let dirFileFI (repo:Repo) udir =
    let dirRoot = Path.Combine(repo.Path.FullName, ".uit", "dirs")
    let (UDir (UPath relative)) = udir
    let dir = 
        if String.IsNullOrEmpty relative then
            dirRoot
        else
            Path.Combine(dirRoot, relative)
    Path.Combine(dir, "dir.txt") |> FileInfo

open System.Globalization

let string2bytes (sbytes: string) =
    let rec tobytelist (str:string) =
        if str.Length >= 2 then
            Byte.Parse( str.[0..1], NumberStyles.HexNumber )::(tobytelist str.[2..])
        else
            assert (str.Length = 0)
            []
    tobytelist sbytes
    |> List.toArray
    
let dirInfo :DirInfo = fun repo udir ->
    let fi = dirFileFI repo udir
    let toFInfo (line :string) =
        let cells = line.Split('\t', 5)
        //let (tp, laststr, entdtstr, hashstr, fname) = (cells.[0], cells.[1], cells.[2], cells.[3], cells.[4])
        match cells with
        | [|tpstr; laststr; entdtstr; hashstr; fname|] ->
            let hash = Hash (string2bytes hashstr)
            let last = DateTime(Int64.Parse laststr)
            let entdt = DateTime(Int64.Parse entdtstr)
            let tp = parseFileType tpstr
            {Type=tp;Hash = hash; FName = fname; LastModified = last; EntryDate = entdt}
        |_ -> failwith "corrupted dir.txt"
    File.ReadLines(fi.FullName)
    |> Seq.map toFInfo
    |> Seq.toList

let computeAndSaveDirInfo = fun repo udir ->
    let fis = computeFInfoList repo udir
    let dirfi = dirFileFI repo udir
    dirfi.Directory |> ensureDir
    finfos2text fis
    |> saveText dirfi
    fis

let createUPath udir fname =
    let (UDir (UPath dir)) = udir
    if dir= "" then
        UPath fname
    else
        UPath (sprintf "%s/%s" dir fname)

let initOneDir :InitOneDir  = fun repo udir ->
    let fis = computeAndSaveDirInfo repo udir
    let fi2pe (fi:FInfo) =
        let upath = createUPath udir fi.FName
        {Type=Instance; Path=upath; LastModified=fi.LastModified; EntryDate=fi.EntryDate }
    let fi2binfo (fi:FInfo) =
        let pe = fi2pe fi
        let bi = toBlobInfo repo fi.Hash
        match bi with
        |ManagedFile mf -> {mf with InstancePathList=pe::mf.InstancePathList }
        |UnmanagedFile -> {Hash =  fi.Hash; InstancePathList=[pe]; ReferencePathList=[]}
    let savemf (mf:ManagedFile) =
        let dest = hashPath mf.Hash
        saveText (toFileInfo repo dest) (mf2text mf)
    fis
    |> List.map fi2binfo
    |> List.iter savemf
    fis

let deleteUitDir (repo:Repo) =
    Directory.Delete(Path.Combine(repo.Path.FullName, ".uit") ,true)


let parentDir (UPath upath) =
    let comps = upath.Split('/')
    if comps.Length = 1 then
        UDir (UPath "")
    else
        comps.[0..(comps.Length-2)]
        |> String.concat "/"
        |> UPath |> UDir

let fileName (UPath upath) =
    let comps = upath.Split('/')
    comps.[comps.Length-1]


let toFInfo :ToFInfo = fun repo upath ->
    let udir = parentDir upath
    let fname = fileName upath
    let fis =
        dirInfo repo udir
        |> List.filter (fun fi -> fi.FName = fname )
    match fis with
    | x::_ -> Some x
    | _-> None


let upath2binfo :UPath2BInfo = fun repo upath ->
    let fi2bi (fi:FInfo) = toBlobInfo repo fi.Hash

    toFInfo repo upath
    |> Option.map fi2bi

let normalDirs (repo:Repo) =
    repo.Path.EnumerateDirectories()
    |> Seq.filter (fun di->
                     match di.Name with
                     | ".uit" -> false
                     | _-> true
                  )

let rootDir = UDir (UPath "")

// fsharplint:disable Hints
let init :Init = fun repo ->
    initOneDir repo rootDir |> ignore
    normalDirs repo
    |> Seq.map (fun di-> (createUDir repo di.FullName))
    |> Seq.map (initOneDir repo)
    |> Seq.toList
    |> ignore

let bi2instances bi =
    match bi with
    | ManagedFile mf-> mf.InstancePathList
    | UnmanagedFile ->[]

let bi2refs bi =
    match bi with
    | ManagedFile mf-> mf.ReferencePathList
    | UnmanagedFile ->[]

let pe2path pe = pe.Path

let bi2allpes bi =
    List.append (bi2instances bi) (bi2refs bi)


let listHash :ListHash =  fun repo ->
    let dir2hash (di:DirectoryInfo) =        
        let bs = di.Name
        let removeExt (name:string) = name.Substring(0, name.Length-4) 
        di.EnumerateFiles()
        |> Seq.filter (fun fi->fi.Name.EndsWith(".txt"))
        |> Seq.map (fun fi->(removeExt fi.Name))
        |> Seq.map (fun rest-> bs+rest)
        |> Seq.map (string2bytes >> Hash)
        |> Seq.toList
    DirectoryInfo(Path.Combine(repo.Path.FullName, ".uit", "hash")).EnumerateDirectories()
    |> Seq.map dir2hash
    |> Seq.concat
    |> Seq.toList

let listMF :ListMF = fun repo ->
    listHash repo
    |> List.map (toBlobInfo repo)
    |> List.map (fun bi -> match bi with |ManagedFile mf->mf|UnmanagedFile -> failwith("never reached"))


let listDupMF : ListDupMF = fun repo->
    listMF repo
    |> List.filter (fun mf-> match mf.InstancePathList with |_::_::_->true|_->false )

// TODO: hash find
// TODO: toInstance
// TODO: toReference
// TODO: uniqIt


//
// Trial code
//

let repo = { Path = DirectoryInfo "/Users/arinokazuma/work/testdata" }


// 
// create MangedFile and save at .uit/hash
//

let h = computeFileHash (FileInfo "/Users/arinokazuma/work/testdata/Uit.fsx")

let fi = FileInfo("/Users/arinokazuma/work/testdata/Uit.fsx")
let entryCreated = DateTime.Now

let pe = {Type=Instance; Path=(UPath "Uit.fsx"); LastModified=fi.LastWriteTime; EntryDate=entryCreated }
let mf = {Hash= h; InstancePathList=[pe]; ReferencePathList=[]}

mf2text mf

let dest = hashPath mf.Hash
saveText (toFileInfo repo dest) (mf2text mf)

// read
toBlobInfo repo h


// 
// create FInfo list and save at .uit/dirs
//

let root = createUDir repo "/Users/arinokazuma/work/testdata/"

let finfos = computeFInfoList repo root

let dirfi = dirFileFI repo root
dirfi.Directory |> ensureDir

finfos2text finfos
|> saveText dirfi


// read
dirInfo repo root


//
// InitOneDir
//

deleteUitDir repo

initOneDir repo root

let sns = createUDir repo "/Users/arinokazuma/work/testdata/sns/"
let imgs = createUDir repo "/Users/arinokazuma/work/testdata/imgs/"

let snsfis = initOneDir repo sns
let imgfis = initOneDir repo imgs


// read
let mikochan = UPath "sns/美子ちゃん.pxv"
toFInfo repo mikochan 

//
// Init
//

deleteUitDir repo
init repo

//
// upath2binfo
//

upath2binfo repo mikochan

upath2binfo repo mikochan
|> Option.map bi2allpes
|> Option.map (List.map pe2path)


upath2binfo repo mikochan
|> Option.map bi2instances
|> Option.map (List.map pe2path)


//
// listHash
//

let mf2upaths mf =
    let ip =
        mf.InstancePathList
        |> List.map (fun pe-> pe.Path)
    let rp =
        mf.ReferencePathList
        |> List.map (fun pe-> pe.Path)
    List.append ip rp

listHash repo
listMF repo
|> List.map mf2upaths |> List.concat

listMF repo
listDupMF repo


let touch (fi:FileInfo) =
    let fs = fi.Create()
    fs.Close()

