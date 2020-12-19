module Action

open Common
open Blob
open FInfo

open System
open System.IO

type ImportOne = Repo -> FileInfo -> UPath.T -> ManagedBlob


// 指定されたUPathをinstanceに。他のinstanceファイルはinstanceのまま。
type ToInstanceOne = Repo -> ManagedBlob -> UPath.T -> ManagedBlob

// 指定されたUPathをRefeerenceに。最後の一つをlinkにしようとする時はエラー。
type ToLinkOne = Repo -> ManagedBlob -> UPath.T -> ManagedBlob

// 指定されたUPathをinstanceに。
// 他のインスタンスは全てlinkにする。
type ToInstance = Repo -> ManagedBlob -> UPath.T -> ManagedBlob

// 重複したinstanceは全てlinkにしてinstanceは一つだけにする。
type UniqIt = Repo -> ManagedBlob -> ManagedBlob

// 指定したディレクトリ下のファイルに対してToInstanceを呼び出す
type ConvDirInstance = Repo -> UDir.T -> FInfo list


// Repo下のファイルの.uit/hash と .uit/dirsを作る。
// まだhashなどが存在しない状態で行われるのでImportとは処理を分けておく
// .uit/dirsを作る都合でファイル単位じゃなくてディレクトリ単位
type InitOneDir = Repo -> UDir.T -> FInfo list


// Repo下のファイルを全てなめて.uit/hashと.uit/dirsを作る
type Init = Repo -> unit

// コマンドいろいろ
type Remove = Repo -> ManagedBlob -> UPath.T -> ManagedBlob
type RemoveTrash = Repo -> ManagedBlob -> unit


let finfo2pe udir (fi:FInfo) =
    let upath = UPath.create udir fi.FName
    {Path=upath; Entry=fi.Entry}

let initOneDir :InitOneDir  = fun repo udir ->
    let fis = computeAndSaveDirInfo repo udir
    let fi2binfo (fi:FInfo) =
        let pe = finfo2pe udir fi
        let bi = fromHash repo fi.Hash
        match bi with
        |ManagedBlob mb -> {mb with InstancePathList=pe::mb.InstancePathList }
        |UnmanagedBlob -> {Hash =  fi.Hash; InstancePathList=[pe]; LinkPathList=[]}
    fis
    |> List.map fi2binfo
    |> List.iter (saveMb repo)
    fis

let upath2binfo :UPath2BInfo = fun repo upath ->
    let fi2bi (fi:FInfo) = fromHash repo fi.Hash

    FInfo.fromUPath repo upath
    |> Option.map fi2bi


let normalDirs (repo:Repo) =
    repo.Path.EnumerateDirectories()
    |> Seq.filter (fun di->
                     match di.Name with
                     | ".uit" -> false
                     | _-> true
                  )

// fsharplint:disable Hints
let init :Init = fun repo ->
    initOneDir repo rootDir |> ignore
    normalDirs repo
    |> Seq.map (fun di-> (UDir.fromAbs repo di.FullName))
    |> Seq.map (initOneDir repo)
    |> Seq.toList
    |> ignore


// without check. internal use only
let toLinkFile repo upath =
    let fi = UPath.toFileInfo repo upath
    fi.Delete()
    toLinkPath upath
    |> createEmpty repo

let toLinkOne :ToLinkOne = fun repo mb upath->
    let (founds, filtered) = findInstance mb upath
    match founds, filtered with
    | [found], _::_ ->
        let parent = parentDir upath
        let dirinfo = dirInfo repo parent
        let fname = UPath.fileName upath
        let (thisfinfos, other) = dirinfo |> List.partition (fun finf -> finf.FName = fname)
        match thisfinfos with
        |[thisfi] -> 
            let newpath = toLinkFile repo upath
            let newname = UPath.fileName newpath
            let newfi = {thisfi with FName = newname; Entry={thisfi.Entry with Type=Link; EntryDate=DateTime.Now}}
            let newfinfos = newfi::other
            let newpe = {Path=newpath; Entry=newfi.Entry}
            saveDirFInfos repo parent newfinfos
            let newMf = 
                { mb with InstancePathList=filtered; LinkPathList= newpe::mb.LinkPathList}
            saveMb repo newMf
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

let pe2finfo hash (pe:PathEntry) =
    {Hash=hash; FName=(UPath.fileName pe.Path); Entry=pe.Entry}

let swapInstance repo instPath linkPath =
    justDeleteFile repo linkPath
    let newInstPath = toInstancePath linkPath
    moveFile repo instPath newInstPath
    let newLnkPath = (toLinkPath instPath)
    createEmpty repo newLnkPath |> ignore
    newLnkPath, newInstPath


let toInstance : ToInstance = fun repo mb target ->
    // hoge.uitlnk を渡すと、hoge.uitlnk.uitlnkにもマッチしちゃうが、まぁいいでしょう。
    let founds, rest = findLink mb target
    match founds with
    |[found] ->
        let headInst = mb.InstancePathList.Head
        let (headLnkPath, newInstPath) = swapInstance repo headInst.Path target

        let newpeInst = {found with Path=newInstPath; Entry={found.Entry with Type=Instance; EntryDate=DateTime.Now}}
        let newpeRef = {headInst with Path=headLnkPath; Entry={headInst.Entry with Type=Link; EntryDate=DateTime.Now}}
        let newmb = {mb with InstancePathList=newpeInst::mb.InstancePathList.Tail; LinkPathList=newpeRef::rest }
        saveMb repo newmb

        let newFInst = pe2finfo mb.Hash newpeInst
        let newFRef = pe2finfo mb.Hash newpeRef
        updateDirInfo repo (parentDir target) newFInst |> ignore
        updateDirInfo repo (parentDir headLnkPath) newFRef |> ignore

        newmb
    |_ -> 
        printfn "upath (%A) is not link" target
        mb


let trashRoot = Path.Combine(".uit", "trash")

let trashUDir = trashRoot |> UDir.fromOSPath

let trashRelativePath fname = Path.Combine(trashRoot, fname)

let trashPath (repo:Repo) fname = Path.Combine(repo.Path.FullName, trashRelativePath(fname) )

let trashPathFI repo fname = FileInfo(trashPath repo fname)

let trashUPath fname = UPath.fromOSPath( trashRelativePath fname )

let findTrashPath repo candidate =
    let fi = trashPathFI repo candidate
    if not fi.Exists then
        fi
    else
        let found =
            seq{ 0..100 }
            |> Seq.map (fun i -> trashPathFI repo (sprintf "%s.%d" candidate i))
            |> Seq.find (fun fi -> not fi.Exists)
        if found = null then
            let msg = sprintf "can't find trash path from %s to %s.%d, probably bug." candidate candidate 100
            failwith(msg)
        found


/// upathのファイルを削除する。
/// リンクならただ削除してhash, dirsを更新するだけ。
/// 他にインスタンスがあってもただ削除するだけ
/// 他にインスタンスが無くリンクがあればリンクの方をinstanceにして削除
/// ただ一つのインスタンスの場合は.uit/trash/ 下にファイルを移動する。
/// rmではファイルの実体がかならず一つはどこかに残る。
/// trashにあるファイルはrmtコマンドで消す。
let remove :Remove = fun repo mb upath ->

    let afterRemove newMb deletedUpath =
        saveMb repo newMb
        removeAndSaveDirInfo repo deletedUpath |> ignore
    
    let moveToTrash mb upath =
        let trash = findTrashPath repo (UPath.fileName upath)
        let trashUPath = trashUPath trash.Name
        ensureDir trash.Directory

        let src = UPath.toFileInfo repo upath
        File.Move(src.FullName, trash.FullName)

        let trashEntry = {mb.InstancePathList.Head.Entry with EntryDate=DateTime.Now}
        let trashPE  = {Path=trashUPath; Entry=trashEntry}

        let newMb = {mb with InstancePathList=[trashPE]}
        let trashFi = {Entry=trashEntry; Hash=mb.Hash; FName = trash.Name }

        updateDirInfo repo trashUDir trashFi |> ignore
        afterRemove newMb upath
        newMb

    let insFounds, insRest = findInstance mb upath
    let lnkFounds, lnkRest = findLink mb upath
    
    match insFounds, lnkFounds, insRest, lnkRest with
    | [_], [], [], [] -> moveToTrash mb upath
    | [_], [], _::_, _ ->
        // instanceで、残りのinstanceもあるケース。
        // 単に引数のファイルを消せば良い
        justDeleteFile repo upath
        let newMb = {mb with InstancePathList=insRest}
        afterRemove newMb upath
        newMb
    |[_], [], [], lnkfirst::_ ->
        // 最後のinstanceでリンクはあるケース。
        // lnkfirstをインスタンスにしてから
        let mb2 = toInstance repo mb lnkfirst.Path

        // リンクになったupathファイルを削除
        let inputLnk = toLinkPath upath
        let lnkFound2, lnkRest2 = findLink mb2 inputLnk
        // lnkFounds2は必ず [inputLnk]
        justDeleteFile repo lnkFound2.Head.Path

        let newMb = {mb2 with LinkPathList=lnkRest2}
        afterRemove newMb lnkFound2.Head.Path
        newMb
    | [], [_], _, _ ->
        // リンクのケース。インスタンスは必ずあるはず。
        // リンクをただ消せば良い。
        justDeleteFile repo upath
        let newMb = {mb with LinkPathList=lnkRest}
        afterRemove newMb upath
        newMb
    | _ -> failwith("Coruppted ManagedFile object")

/// トラッシュにあるblobを本当に削除する。
/// 削除は安全のため、トラッシュにだけある状態でしか行えない。
let removeTrash :RemoveTrash = fun repo mb ->

    let isTrash upath =
        UPath.pred (fun value->value.StartsWith(trashRoot)) upath

    match mb.InstancePathList, mb.LinkPathList with
    | [trash], [] ->
        if not (isTrash trash.Path) then
            failwith("Not trash blob2")
        justDeleteFile repo trash.Path
        removeAndSaveDirInfo repo trash.Path |> ignore
        removeMbFile repo mb.Hash
    | _, _-> failwith("Not trash blob.")

// TODO: import
// TODO: cp
// TODO: mv

