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

type ComputeHash = Repo -> UPath -> Hash
type ToBlobInfo = Repo -> Hash -> BlobInfo
type PathToBlobInfo = Repo -> UPath -> BlobInfo

type ImportOne = Repo -> FileInfo -> UPath -> ManagedFile

type ListHash = Repo -> Hash list
type ListBlobInfo = Repo -> ManagedFile list

type SaveBlobInfo = Repo -> ManagedFile -> unit

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

// Repo下のファイルの.uit/hash と .uit/dirsを作る。
// まだhashなどが存在しない状態で行われるのでImportとは処理を分けておく
// .uit/dirsを作る都合でファイル単位じゃなくてディレクトリ単位
type InitOneDir = Repo -> UDir -> FInfo list


type DirInfo = Repo -> UDir -> FInfo list
type ToFInfo = Repo -> UPath -> FInfo option

type FInfos2Text = FInfo list -> string
type ManagedFileToText = ManagedFile -> string

type SaveText = FileInfo -> string -> unit



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
    

let saveText (fi: FileInfo) text =
    ensureDir fi.Directory
    File.WriteAllText("temp.dat", text)
    File.Move("temp.dat", fi.FullName, true)


let toBlobInfo :ToBlobInfo = fun repo hash ->
    let dir = hashPath hash
    let fi = toFileInfo repo dir
    let toPE (cells : string array) =
        {Path=(UPath cells.[2]); LastModified=DateTime(Int64.Parse cells.[0]); EntryDate=DateTime(Int64.Parse cells.[1])}
    let toIoR (line: string) =
        let cells = line.Split('\t', 4)
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
        | [|tp; laststr; entdtstr; hashstr; fname|] ->
            let hash = Hash (string2bytes hashstr)
            let last = DateTime(Int64.Parse laststr)
            let entdt = DateTime(Int64.Parse entdtstr)
            let fent = {Hash = hash; FName = fname; LastModified = last; EntryDate = entdt}
            if tp = "1" then
                InstanceFile fent
            else
                ReferenceFile fent
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

let finfo2fie finfo =
    match finfo with
    |InstanceFile fie -> fie
    |ReferenceFile fie -> fie

let initOneDir :InitOneDir  = fun repo udir ->
    let fis = computeAndSaveDirInfo repo udir
    let fi2pe (fie:FInfoEntry) =
        let upath = createUPath udir fie.FName
        {Path=upath; LastModified=fie.LastModified; EntryDate=fie.EntryDate }
    let fi2blobInfo (fie:FInfoEntry) =
        let pe = fi2pe fie
        let bi = toBlobInfo repo fie.Hash
        match bi with
        |ManagedFile mf -> {mf with InstancePathList=pe::mf.InstancePathList }
        |UnmanagedFile -> {Hash =  fie.Hash; InstancePathList=[pe]; ReferencePathList=[]}
    let savemf (mf:ManagedFile) =
        let dest = hashPath mf.Hash
        saveText (toFileInfo repo dest) (mf2text mf)
    fis
    |> List.map (finfo2fie >> fi2blobInfo)
    |> List.iter savemf
    fis

let deleteUitDir (repo:Repo) =
    Directory.Delete(Path.Combine(repo.Path.FullName, ".uit") ,true)



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

let pe = {Path=(UPath "Uit.fsx"); LastModified=fi.LastWriteTime; EntryDate=entryCreated }
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


let arr = [|1..5|]

arr

let [|a; b; c; d; e|] = arr
a