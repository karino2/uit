module Action

open Common
open Blob
open FInfo

open System
open System.IO

type ImportOne = Repo -> FileInfo -> UPath -> ManagedBlob


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


// Repo下のファイルの.uit/hash と .uit/dirsを作る。
// まだhashなどが存在しない状態で行われるのでImportとは処理を分けておく
// .uit/dirsを作る都合でファイル単位じゃなくてディレクトリ単位
type InitOneDir = Repo -> UDir -> FInfo list


// Repo下のファイルを全てなめて.uit/hashと.uit/dirsを作る
type Init = Repo -> unit

// コマンドいろいろ
type Remove = Repo -> ManagedBlob -> UPath -> ManagedBlob

let finfo2pe udir (fi:FInfo) =
    let upath = createUPath udir fi.FName
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
    |> Seq.map (fun di-> (createUDir repo di.FullName))
    |> Seq.map (initOneDir repo)
    |> Seq.toList
    |> ignore


// without check. internal use only
let toLinkFile repo upath =
    let fi = toFileInfo repo upath
    fi.Delete()
    toLinkPath upath
    |> createEmpty repo

let toLinkOne :ToLinkOne = fun repo mb upath->
    let (founds, filtered) = findInstance mb upath
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
    {Hash=hash; FName=(fileName pe.Path); Entry=pe.Entry}

let swapInstance repo instPath linkPath =
    removeFile repo linkPath
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
        printfn "upath (%A) is not instance" target
        mb


let deleteFileInner repo upath =
    let fi = toFileInfo repo upath
    File.Delete(fi.FullName)

let trashUDir = Path.Combine(".uit", "trash") |> UPath |> UDir

let trashRelativePath fname = Path.Combine(".uit", "trash", fname)

let trashPath (repo:Repo) fname = Path.Combine(repo.Path.FullName, trashRelativePath(fname) )

let trashPathFI repo fname = FileInfo(trashPath repo fname)

let trashUPath fname = UPath( trashRelativePath fname )

let findTrashPath repo candidate =
    let fi = trashPathFI repo candidate
    if fi.Exists then
        fi
    else
        let found =
            seq{ 0..100 }
            |> Seq.map (fun i -> trashPathFI repo (sprintf "%s.%d" candidate i))
            |> Seq.find (fun fi -> fi.Exists)
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
    let udir = parentDir upath

    let afterRemove newMb deletedUpath =
        saveMb repo newMb
        removeAndSaveDirInfo repo udir deletedUpath |> ignore
    
    let moveToTrash mb upath =
        let trash = findTrashPath repo (fileName upath)
        let trashUPath = trashUPath trash.Name
        ensureDir trash.Directory

        let src = toFileInfo repo upath
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
        deleteFileInner repo upath
        let newMb = {mb with InstancePathList=insRest}
        afterRemove newMb upath
        newMb
    |[_], [], [], lnkfirst::_ ->
        // 最後のinstanceのケース。
        // lnkfirstをインスタンスにしてから
        let mb2 = toInstance repo mb lnkfirst.Path

        // リンクになったupathファイルを削除
        let inputLnk = toLinkPath upath
        let lnkFound2, lnkRest2 = findLink mb2 inputLnk
        // lnkFounds2は必ず [inputLnk]
        deleteFileInner repo lnkFound2.Head.Path

        let newMb = {mb2 with LinkPathList=lnkRest2}
        afterRemove newMb lnkFound2.Head.Path
        newMb
    | [], [_], _, _ ->
        deleteFileInner repo upath
        let newMb = {mb with LinkPathList=lnkRest}
        afterRemove newMb upath
        newMb
    | _ -> failwith("Coruppted ManagedFile object")

// TODO: rmt
// TODO: import
// TODO: cp
// TODO: mv

