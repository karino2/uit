open System

type Hash = Hash of byte array

type UPath = UPath of string
type UDir = UDir of UPath
type OSPath = OSPath of string
type Repo = { Path: OSPath }

type PathEntry = {
    Path : UPath
    LastModified : DateTime
    EntryDate: DateTime
}

// .uit/hash/xx/yyyyyyyyyyy.txt にかかれている情報
// hashはパスから取り出す。
// 各行はそのハッシュのblobに対応したパスの情報を表す。
// 一行は、
// LastModified EntryDate   UPath
// の形式で、LastModifiedは対応するblobをimportした時のfileのlastmodified。
// EntryDateはこの行を更新した時の日時
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
    PathEntry : PathEntry
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

let computeFileHath ospath =
    let (OSPath bpath) = ospath
    Hash (computeRawFileHash sha bpath)







// let computeHash repo upath =
//   osPathToHash (toOSPath repo upath)


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


let toBlobInfo repo hash =
    let hashfile = hashPath hash
    let osfinfo = fileinfo (toOSPath repo hashfile)
    if osfinfo.Exists then
        ManagedFile {Hash=hash; InstancePathList=[]; ReferencePathList=[]}
    else
        UnmanagedFile



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