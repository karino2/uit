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

// .uit/hash/xx/yyyyyyyyyyy.txt にかかれている情報
// hashはパスから取り出す。
// 各行はそのハッシュのblobに対応したパスの情報を表す。
// 一行は、
// t LastModified EntryDate   fname
// の形式で、LastModifiedは対応するblobをimportした時のfileのlastmodified。
// EntryDateはこの行を更新した時の日時
// tはinstanceなら1、linkなら2。
type ManagedBlob = { 
    Hash : Hash
    InstancePathList : PathEntry list
    LinkPathList : PathEntry list
}

type BlobInfo = 
| ManagedBlob of ManagedBlob
| UnmanagedBlob

// ".uit/dirs/"下にファイルのパスに対応した情報がある。
// dirsの下にディレクトリ構造そのままコピーしたディレクトリ構造があり、
// 目的のディレクトリエントリにはdir.txtというファイルがある。
// 例えば hoge/ika/test.txtというファイルの情報は、
// .uit/dirs/hoge/ika/dir.txt にある。
// このdir.txtの中身の各行はFInfoを表し、
// tp LastModified EntryDate Hash ファイル名
// となっている。tpはinstanceなら1, linkなら2
// EntryDateはhash下と同じ物を使う。
type FInfo = {
    Entry : Entry
    Hash: Hash
    FName: string
}


type ComputeHash = Repo -> UPath -> Hash
type ToBInfo = Repo -> Hash -> BlobInfo
type PathToBInfo = Repo -> UPath -> BlobInfo

type ImportOne = Repo -> FileInfo -> UPath -> ManagedBlob

type ListHash = Repo -> Hash list
// 文字列に一致するハッシュの一覧
type ListHashWith = Repo -> string -> Hash list
type ListMF = Repo -> ManagedBlob list
type ListDupMF = Repo -> ManagedBlob list

type SaveBInfo = Repo -> ManagedBlob -> unit

type InstancePaths = ManagedBlob -> PathEntry list
type LinkPaths = ManagedBlob -> PathEntry list

// 指定されたUPathをinstanceに。他のinstanceファイルはinstanceのまま。
type ToInstanceOne = Repo -> ManagedBlob -> UPath -> ManagedBlob
// 指定されたUPathをRefeerenceに。最後の一つをlinkにしようとする時はエラー。
type ToLinkOne = Repo -> ManagedBlob -> UPath -> ManagedBlob

// 指定されたUPathをinstanceに。
// 他のインスタンスは全てlinkにする。
type ToInstance = Repo -> ManagedBlob -> UPath -> ManagedBlob

// 重複したinstanceは全てlinkにしてinstanceは一つだけにする。
type UniqIt = Repo -> ManagedBlob -> ManagedBlob

// 指定したディレクトリ下のファイルに対してToInstanceを呼び出す
type ConvDirInstance = Repo -> UDir -> FInfo list

type Paths = Repo -> Hash -> UPath list

type SaveText = FileInfo -> string -> unit

//
// dirs下関連
//

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
type ManagedBlobToText = ManagedBlob -> string


// コマンドいろいろ
type Remove = Repo -> ManagedBlob -> UPath -> ManagedBlob


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


let mb2text :ManagedBlobToText = fun mb->
    let totext tp (pe: PathEntry) =
        let (UPath path) = pe.Path
        sprintf "%d\t%d\t%d\t%s\n"  tp pe.Entry.LastModified.Ticks pe.Entry.EntryDate.Ticks path
    let instances = mb.InstancePathList |> List.map (totext 1)
    let links = mb.LinkPathList |> List.map (totext 2)
    List.append instances links |> List.reduce (+)


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

let toBInfo :ToBInfo = fun repo hash ->
    let dir = hashPath hash
    let fi = toFileInfo repo dir
    let toIoR (line: string) =
        let cells = line.Split('\t', 4)
        let tp = parseFileType cells.[0]
        {Path=(UPath cells.[3]); Entry={Type = tp; LastModified=DateTime(Int64.Parse cells.[1]); EntryDate=DateTime(Int64.Parse cells.[2])}}
    if fi.Exists then
        let onlyIorR icase rcase =
            fun (m:PathEntry) ->
                match m.Entry.Type with
                |Instance _-> icase
                |Link _ -> rcase
        let ret = 
            File.ReadLines fi.FullName
            |> Seq.map toIoR
        let is = ret |> Seq.filter (onlyIorR true false) |> Seq.toList
        let rs = ret |> Seq.filter (onlyIorR false true) |> Seq.toList
        ManagedBlob { Hash = hash; InstancePathList=is; LinkPathList=rs }
    else
        UnmanagedBlob

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
    {Hash = hash; FName=fi.Name; Entry={Type=Instance; LastModified=fi.LastWriteTime; EntryDate=DateTime.Now}}

let finfo2text finfo =
    let fmt tp (lastmod:DateTime) (entdt:DateTime) hash fname =
        sprintf "%d\t%d\t%d\t%s\t%s" tp lastmod.Ticks entdt.Ticks (hash2string hash) fname
    let format tp finfo =
        fmt tp finfo.Entry.LastModified finfo.Entry.EntryDate finfo.Hash finfo.FName
    match finfo.Entry.Type with
    | Instance -> format 1 finfo
    | Link -> format 2 finfo

let finfos2text finfos =
    finfos
    |> List.map finfo2text
    |> String.concat "\n"

let computeFInfoList repo udir =
    let di = toDirInfo repo udir
    di.EnumerateFiles()
    |> Seq.map computeFInfo
    |> Seq.toList

let dirRootStr (repo:Repo) =
    Path.Combine( repo.Path.FullName, ".uit", "dirs")

let dirFileFI repo udir =
    let dirRoot = dirRootStr repo
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
            {Hash = hash; FName = fname; Entry={Type=tp;LastModified = last; EntryDate = entdt}}
        |_ -> failwith "corrupted dir.txt"
    File.ReadLines(fi.FullName)
    |> Seq.map toFInfo
    |> Seq.toList

let saveDirFInfos repo udir fis =
    let dirfi = dirFileFI repo udir
    dirfi.Directory |> ensureDir
    finfos2text fis
    |> saveText dirfi

let computeAndSaveDirInfo = fun repo udir ->
    let fis = computeFInfoList repo udir
    saveDirFInfos repo udir fis
    fis

let createUPath udir fname =
    let (UDir (UPath dir)) = udir
    if dir= "" then
        UPath fname
    else
        UPath (sprintf "%s/%s" dir fname)

let saveMf repo (mb:ManagedBlob) =
    let dest = hashPath mb.Hash
    saveText (toFileInfo repo dest) (mb2text mb)

let finfo2pe udir (fi:FInfo) =
    let upath = createUPath udir fi.FName
    {Path=upath; Entry=fi.Entry}

let initOneDir :InitOneDir  = fun repo udir ->
    let fis = computeAndSaveDirInfo repo udir
    let fi2binfo (fi:FInfo) =
        let pe = finfo2pe udir fi
        let bi = toBInfo repo fi.Hash
        match bi with
        |ManagedBlob mb -> {mb with InstancePathList=pe::mb.InstancePathList }
        |UnmanagedBlob -> {Hash =  fi.Hash; InstancePathList=[pe]; LinkPathList=[]}
    fis
    |> List.map fi2binfo
    |> List.iter (saveMf repo)
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

let LinkExt = ".uitlnk"

let toFInfo :ToFInfo = fun repo upath ->
    let udir = parentDir upath
    let fname = fileName upath
    let eqname name (fi:FInfo) =
        name = fi.FName
    let fis = dirInfo repo udir

    let found = fis |> List.filter (eqname fname)
    match found with
    | x::_ -> Some x
    | _-> 
        let found2 = fis |> List.filter (eqname (fname + LinkExt))
        match found2 with
        | y::_ -> Some y
        | _-> None


let upath2binfo :UPath2BInfo = fun repo upath ->
    let fi2bi (fi:FInfo) = toBInfo repo fi.Hash

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
    | ManagedBlob mb-> mb.InstancePathList
    | UnmanagedBlob ->[]

let bi2links bi =
    match bi with
    | ManagedBlob mb-> mb.LinkPathList
    | UnmanagedBlob ->[]

let pe2path pe = pe.Path

let bi2allpes bi =
    List.append (bi2instances bi) (bi2links bi)

let trimEnd (pat:string) (target:string) =
    if target.EndsWith(pat) then
        target.Remove(target.LastIndexOf pat)
    else
        target


let removeTxtExt (name:string) = trimEnd ".txt" name

let hashRootStr (repo:Repo) =
    Path.Combine(repo.Path.FullName, ".uit", "hash")

let listHash :ListHash =  fun repo ->
    let dir2hash (di:DirectoryInfo) =        
        let bs = di.Name
        di.EnumerateFiles()
        |> Seq.filter (fun fi->fi.Name.EndsWith(".txt"))
        |> Seq.map (fun fi->(removeTxtExt fi.Name))
        |> Seq.map (fun rest-> bs+rest)
        |> Seq.map (string2bytes >> Hash)
        |> Seq.toList
    DirectoryInfo(hashRootStr(repo)).EnumerateDirectories()
    |> Seq.map dir2hash
    |> Seq.concat
    |> Seq.toList

let listMF :ListMF = fun repo ->
    listHash repo
    |> List.map (toBInfo repo)
    |> List.map (fun bi -> match bi with |ManagedBlob mb->mb|UnmanagedBlob -> failwith("never reached"))


let listDupMF : ListDupMF = fun repo->
    listMF repo
    |> List.filter (fun mb-> match mb.InstancePathList with |_::_::_->true|_->false )

let listHashWith : ListHashWith = fun repo hashstr ->
    if hashstr.Length < 2 then
        []
    else
        let dirname = hashstr.[0..1]
        let dirRoot = hashRootStr repo
        let dir = DirectoryInfo(Path.Combine(dirRoot, dirname))
        printfn "%A %A" dir dir.Exists
        if not dir.Exists then
            []
        else
            dir.EnumerateFiles()
            |> Seq.filter (fun fi -> fi.Name.StartsWith(hashstr.[2..]))
            |> Seq.map (fun fi-> dirname + (removeTxtExt fi.Name))
            |> Seq.map (string2bytes >> Hash)
            |> Seq.toList

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

// without check. internal use only
let toLinkFile repo upath =
    let fi = toFileInfo repo upath
    fi.Delete()
    toLinkPath upath
    |> createEmpty repo

let toLinkOne :ToLinkOne = fun repo mb upath->
    let (founds, filtered) = mb.InstancePathList |> List.partition (fun x->x.Path = upath)
    match founds, filtered with
    | [found], _::_ ->
        let parent = parentDir upath
        let dirinfo = dirInfo repo parent
        let fname = fileName upath
        let (thisfinfos, other) = dirinfo |> List.partition (fun finf -> finf.FName = fname)
        match thisfinfos with
        |[thisfi] -> 
            let newpath = toLinkFile repo upath
            let newname = fileName newpath
            let newfi = {thisfi with FName = newname; Entry={thisfi.Entry with Type=Link; EntryDate=DateTime.Now}}
            let newfinfos = newfi::other
            let newpe = {Path=newpath; Entry=newfi.Entry}
            saveDirFInfos repo parent newfinfos
            let newMf = 
                { mb with InstancePathList=filtered; LinkPathList= newpe::mb.LinkPathList}
            saveMf repo newMf
            newMf
        |_ -> failwith("upath does not in dirs.txt")        
    | [], _ -> failwith("upath not found")
    | _, [] -> failwith("only one instance and try changing to link")
    | _, _ -> failwith("never happend (like same upath twice, etc.)")


// instanceなPEsをlinkにする。
// instanceを一つは残すのは呼ぶ側の責任
let makePEListLinks repo mb (pelist:PathEntry list) =
    pelist |> List.map (fun pe->pe.Path) |> List.fold (toLinkOne repo) mb


let uniqIt : UniqIt = fun repo mb ->
    match mb.InstancePathList with
    |first::rest ->
        makePEListLinks repo mb rest
    |_ -> mb



let peEqual upath pe =
    upath = pe.Path || (toLinkPath upath) = pe.Path

let moveFile repo upath1 upath2 =
    let fi1 = toFileInfo repo upath1
    let fi2 = toFileInfo repo upath2
    File.Move(fi1.FullName, fi2.FullName)    

let removeFile repo upath =
    let fi = toFileInfo repo upath
    File.Delete(fi.FullName)

let pe2finfo hash (pe:PathEntry) =
    {Hash=hash; FName=(fileName pe.Path); Entry=pe.Entry}

let swapInstance repo instPath linkPath =
    removeFile repo linkPath
    let newInstPath = toInstPath linkPath
    moveFile repo instPath newInstPath
    let newLnkPath = (toLinkPath instPath)
    createEmpty repo newLnkPath |> ignore
    newLnkPath, newInstPath

let updateDirInfo repo udir newFi =
    let rest =
        dirInfo repo udir
        |> List.filter (fun finfo-> finfo.Hash <> newFi.Hash)
    let newFis = newFi::rest
    saveDirFInfos repo udir newFis
    newFis

let toInstance : ToInstance = fun repo mb target ->
    // hoge.uitlnk を渡すと、hoge.uitlnk.uitlnkにもマッチしちゃうが、まぁいいでしょう。
    let founds, rest =
         mb.LinkPathList |> List.partition (peEqual target)
    match founds with
    |[found] ->
        let headInst = mb.InstancePathList.Head
        let (headLnkPath, newInstPath) = swapInstance repo headInst.Path target

        let newpeInst = {found with Path=newInstPath; Entry={found.Entry with Type=Instance; EntryDate=DateTime.Now}}
        let newpeRef = {headInst with Path=headLnkPath; Entry={headInst.Entry with Type=Link; EntryDate=DateTime.Now}}
        let newmb = {mb with InstancePathList=newpeInst::mb.InstancePathList.Tail; LinkPathList=newpeRef::rest }
        saveMf repo newmb

        let newFInst = pe2finfo mb.Hash newpeInst
        let newFRef = pe2finfo mb.Hash newpeRef
        updateDirInfo repo (parentDir target) newFInst |> ignore
        updateDirInfo repo (parentDir headLnkPath) newFRef |> ignore

        newmb
    |_ -> 
        printfn "upath (%A) is not instance" target
        mb

let findInstance mb upath =
    mb.InstancePathList |> List.partition  (fun x->x.Path = upath)

let partitionPE target pelist =
    List.partition (peEqual target) pelist

(*
/// upathのファイルを削除する。
/// リンクならただ削除してhash, dirsを更新するだけ。
/// 他にインスタンスがあってもただ削除するだけ
/// 他にインスタンスが無くリンクがあればリンクの方をinstanceにして削除
/// ただ一つのインスタンスの場合は.uit/trash/ 下にファイルを移動する。
/// rmではファイルの実体がかならず一つはどこかに残る。
/// trashにあるファイルはrmtコマンドで消す。
let remove :Remove = fun repo mb upath ->
    let insFounds, insRest = mb.InstancePathList |> List.partition (peEqual target)
    let insFounds, insRest = mb.InstancePathList |> List.partition (peEqual target)
*)


// TODO: rm
// TODO: rmt
// TODO: import
// TODO: cp
// TODO: mv


//
// Trial code
//

let repo = { Path = DirectoryInfo "/Users/arinokazuma/work/testdata" }
let mikochan = UPath "sns/美子ちゃん.pxv"

//
// Init
//

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

let mb2upaths mb =
    let ip =
        mb.InstancePathList
        |> List.map (fun pe-> pe.Path)
    let rp =
        mb.LinkPathList
        |> List.map (fun pe-> pe.Path)
    List.append ip rp

listHash repo
listMF repo
|> List.map mb2upaths |> List.concat

listMF repo
listDupMF repo


listHashWith repo "2b0b"
listMF repo


//
//  ToLinkOne, trial
//

// init repo
// let mikochan = UPath "sns/美子ちゃん.pxv"

let dups = listDupMF repo
uniqIt repo dups.Head
listDupMF repo


let dispMb (mb:ManagedBlob) =
    printfn "Hash: %s" (hash2string mb.Hash)
    printf "Inst: "
    mb.InstancePathList |> List.iter (fun pe->printf "%A " pe.Path)
    printf "\nLinks: "
    mb.LinkPathList |> List.iter (fun pe->printf "%A " pe.Path)
    printfn ""

let lsa repo upath =
    let opbinfo = toFInfo repo upath
                |> Option.map (fun fi->fi.Hash)
                |> Option.map (toBInfo repo)
    match opbinfo with
    | (Some (ManagedBlob mb)) -> dispMb mb
    | _ -> ()

let lsmb repo upath =
    let opbinfo = toFInfo repo upath
                |> Option.map (fun fi->fi.Hash)
                |> Option.map (toBInfo repo)
    match opbinfo with
    | (Some (ManagedBlob mb)) -> mb
    | _ -> failwith("not managed path")


let mb = lsmb repo (UPath "imgs/美子ちゃん.pxv")

toInstance repo mb (UPath "imgs/美子ちゃん.pxv.uitlnk")

lsa repo (UPath "imgs/美子ちゃん.pxv")

toInstance repo mb (UPath "sns/美子ちゃん.pxv.uitlnk")

lsa repo (UPath "imgs/美子ちゃん.pxv")

mb


(*
想定するコマンドラインの使い方を考える。

cd repodir
uit init
uit lsdup
uit uniqit mp3/
uit uniqit -all
uit inst study/language/
uit lsh
uit info 2b0b
uit inst sns/img1.png

uit ls sns/img1.png

// -lで他のリンク先の情報も見る
uit ls -l sns/img1.png

// すでにあったらlinkとしてインポート。
uit import /somewhere/file.mp3 music/file.mp3

// .uit/dirs や .uit/hash を更新しつつ移動
uit mv sutudy/language/roman study/roman

// linkとしてコピーして実体はコピーしない
uit cp mp3/roman/somefile.mp3 study/roman/somefile.mp3

// -iで新しく追加した方をinstanceにする
uit cp -i mp3/roman/somefile.mp3 study/roman/somefile.mp3

*)
