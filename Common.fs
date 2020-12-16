module Common

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
| Link

type Entry = {
    Type: FileType
    LastModified : DateTime
    EntryDate: DateTime
}

type PathEntry = {
    Entry: Entry
    Path : UPath
}

type ComputeHash = Repo -> UPath -> Hash

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


let hashDir hash =
    let (Hash bytes) = hash
    (UPath (sprintf ".uit/hash/%02x/" bytes.[0]))


let hashPath hash =
    let (Hash bytes) = hash
    let (UPath dir) = hashDir hash
    (UPath (sprintf "%s%s.txt" dir (bytes2string bytes.[1..])))


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
    | "2" -> Link
    | _ -> failwith "invalid data"


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

let createUPath udir fname =
    let (UDir (UPath dir)) = udir
    if dir= "" then
        UPath fname
    else
        UPath (sprintf "%s/%s" dir fname)

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

let LinkExt = ".uitlnk"

let rootDir = UDir (UPath "")

let trimEnd (pat:string) (target:string) =
    if target.EndsWith(pat) then
        target.Remove(target.LastIndexOf pat)
    else
        target

let removeTxtExt (name:string) = trimEnd ".txt" name


let touch (fi:FileInfo) =
    let fs = fi.Create()
    fs.Close()

let toLinkPath upath =
    let (UPath v) = upath
    (UPath (v + LinkExt))

let toInstPath upath =
    let (UPath v) = upath
    (UPath (trimEnd LinkExt v))

let createEmpty repo upath =
    let fi = toFileInfo repo upath
    touch fi
    upath


let moveFile repo upath1 upath2 =
    let fi1 = toFileInfo repo upath1
    let fi2 = toFileInfo repo upath2
    File.Move(fi1.FullName, fi2.FullName)    

let removeFile repo upath =
    let fi = toFileInfo repo upath
    File.Delete(fi.FullName)

let peEqual upath pe =
    upath = pe.Path || (toLinkPath upath) = pe.Path
