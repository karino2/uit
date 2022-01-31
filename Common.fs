module Common

open System
open System.IO

type Hash = Hash of byte array

// このPathは最後のスラッシュは含まない
type Repo = { Path: DirectoryInfo }

let deleteDotUit (repo:Repo) =
    Directory.Delete(Path.Combine(repo.Path.FullName, ".uit") ,true)

let diEquals (di1:DirectoryInfo) (di2:DirectoryInfo) =
    di1.FullName.TrimEnd(Path.DirectorySeparatorChar) = di2.FullName.TrimEnd(Path.DirectorySeparatorChar)

let ensureDir (di: DirectoryInfo) =
    if not di.Exists then
        Directory.CreateDirectory di.FullName |> ignore

let trimEnd (pat:string) (target:string) =
    if target.EndsWith(pat) then
        target.Remove(target.LastIndexOf pat)
    else
        target


// スラッシュ無しで始まりスラッシュ無しで終わる。
// rootは""で表す。
module UDir =
    type T = V of string
    let fromUit str = V str

    let fromOSPath str = V str

    let fromDI (repo:Repo) (from:DirectoryInfo) =
        let relative =
            if diEquals repo.Path from then
                ""
            else
                let fromAbs = trimEnd "/" from.FullName
                let repoAbs = repo.Path.FullName + "/"
                if not (fromAbs.StartsWith repoAbs) then
                    sprintf "from is not under repo: from=%s, repo=%s\n" fromAbs repoAbs
                    |> failwith
                fromAbs.Substring repoAbs.Length
        relative
        |> fromUit

    let child (V parent) childName = fromUit (sprintf "%s/%s" parent childName)

    // OS path separator区切り
    let toOSPath (V str) = str

    // スラッシュ区切り
    let toUitStr (V str) = str

    let toDI repo udir =
        let path = toOSPath udir
        Path.Combine(repo.Path.FullName, path).TrimEnd(Path.DirectorySeparatorChar)
        |> DirectoryInfo

    let fromAbs repo (abspath:string) =
        abspath.TrimEnd Path.DirectorySeparatorChar
        |> DirectoryInfo 
        |> fromDI repo 

    let ensureExists repo udir =
        ensureDir (toDI repo udir)

// パス区切りはスラッシュで。uitのrootからの相対パスを表す。
module UPath =
    type T = V of string
    /// osのpath separator区切りによるfactory
    let fromOSPath str = V str

    /// uit内のスラッシュ区切りによるfactory
    let fromUit str = V str

    let toUitStr (V str) = str

    let fromFileInfo (repo:Repo) (from:FileInfo) =
        let fromAbs = from.FullName
        let repoAbs = repo.Path.FullName + "/"
        if not (fromAbs.StartsWith repoAbs) then
            failwith "from is not under repo"
        V (fromAbs.Substring repoAbs.Length)


    let toFileInfo (repo:Repo) (V value) =
        FileInfo(Path.Combine(repo.Path.FullName, value))

    let fileName upath =
        let upathstr = toUitStr upath
        let comps = upathstr.Split('/')
        comps.[comps.Length-1]

    let conv func (V str) =
        fromUit (func str)
    
    let addExtent extent upath =
        conv (fun str->str+extent) upath

    let pred func (V str) =
        (func str)

    let create udir fname =
        let dir = UDir.toUitStr udir
        if dir= "" then
            fromUit fname
        else
            fromUit (sprintf "%s/%s" dir fname)


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
    Path : UPath.T
}

type ComputeHash = Repo -> UPath.T -> Hash

type SaveText = FileInfo -> string -> unit

//
// Implementation
// 

open System.Security.Cryptography

let sha = SHA256.Create()

let computeRawFileHash (sha:SHA256) (path:string) =
    use reader = new FileStream(path, FileMode.Open)
    sha.ComputeHash reader

let computeFileHash (fi:FileInfo) =
    Hash (computeRawFileHash sha fi.FullName)

let bytes2string (bytes: byte[])=
    bytes |> Array.map (sprintf "%02x") |> String.concat ""

let hash2string (Hash hash) =
    bytes2string hash

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
    (UDir.fromUit (sprintf ".uit/hash/%02x" bytes.[0]))


let hashPath hash =
    let (Hash bytes) = hash
    let dir = hashDir hash |> UDir.toUitStr
    (UPath.fromUit (sprintf "%s/%s.txt" dir (bytes2string bytes.[1..])))


    

let saveText :SaveText = fun fi text ->
    ensureDir fi.Directory
    File.WriteAllText("temp.dat", text)
    File.Move("temp.dat", fi.FullName, true)

let parseFileType str =
    match str with
    | "1" -> Instance
    | "2" -> Link
    | _ -> failwith "invalid data"


let rootDir = UDir.fromUit ""

let parentDir upath =
    let pathstr = UPath.toUitStr upath
    let comps = pathstr.Split('/')
    if comps.Length = 1 then
        rootDir
    else
        comps.[0..(comps.Length-2)]
        |> String.concat "/"
        |> UDir.fromUit

let LinkExt = ".uitlnk"

let touch (fi:FileInfo) =
    let fs = fi.Create()
    fs.Close()

let toLinkPath upath =
    UPath.addExtent LinkExt upath

let toInstancePath upath =
    UPath.conv (trimEnd LinkExt) upath

let createEmpty repo upath =
    let fi = UPath.toFileInfo repo upath
    touch fi
    upath

let assertFileNotExists repo upath =
    let fi = UPath.toFileInfo repo upath
    if fi.Exists then
        failwith (sprintf "file %A is (wrongly) exists." fi)

let assertDirNotExists repo udir =
    let di = UDir.toDI repo udir
    if di.Exists then
        failwith (sprintf "Directory %A is (wrongly) exists." di)

let moveFile repo upath1 upath2 =
    let fi1 = UPath.toFileInfo repo upath1
    let fi2 = UPath.toFileInfo repo upath2
    File.Move(fi1.FullName, fi2.FullName)    

// メタ情報の更新など一切せずにただファイルを削除する。
let justDeleteFile repo upath =
    let fi = UPath.toFileInfo repo upath
    File.Delete(fi.FullName)

let peEqual upath pe =
    upath = pe.Path || (toLinkPath upath) = pe.Path


/// .DS_StoreをskipするEnumerateFiles()、ただし結果はlistで返す
let listFiles (di:DirectoryInfo) =
    di.EnumerateFiles()
    |> Seq.filter (fun fi-> fi.Name <> ".DS_Store")
    |> Seq.toList