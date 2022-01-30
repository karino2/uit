module Blob

open Common
open System
open System.IO
// fsharplint:disable Hints

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

module Blob =
    let fromHash repo hash =
        let dir = hashPath hash
        let fi = UPath.toFileInfo repo dir
        let toIoR (line: string) =
            let cells = line.Split('\t', 4)
            let tp = parseFileType cells.[0]
            {Path=(UPath.fromUit cells.[3]); Entry={Type = tp; LastModified=DateTime(Int64.Parse cells.[1]); EntryDate=DateTime(Int64.Parse cells.[2])}}
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

    /// 必ず存在する事が保証されてるケース。ManagedBlobを返す
    let fromHashMB repo hash =
        match (fromHash repo hash) with
        |ManagedBlob mb -> mb
        |_ -> failwith(sprintf "Not exist %s" (hash2string hash))

    let toText mb =
        let toTextOne tp (pe: PathEntry) =
            let path = UPath.toUitStr pe.Path
            sprintf "%d\t%d\t%d\t%s\n"  tp pe.Entry.LastModified.Ticks pe.Entry.EntryDate.Ticks path
        let instances = mb.InstancePathList |> List.map (toTextOne 1)
        let links = mb.LinkPathList |> List.map (toTextOne 2)
        List.append instances links |> List.reduce (+)
    
    let save repo mb =
        let dest = hashPath mb.Hash
        saveText (UPath.toFileInfo repo dest) (toText mb)

    let removeByHash repo hash =
        let dest = hashPath hash
        justDeleteFile repo dest
        let dir = UDir.toDI repo (parentDir dest)
        let files = listFiles dir |> List.toArray
        if files.Length = 0 then
            dir.Delete()
    
    let instances bi =
        match bi with
        | ManagedBlob mb-> mb.InstancePathList
        | UnmanagedBlob ->[]

    let links bi =
        match bi with
        | ManagedBlob mb-> mb.LinkPathList
        | UnmanagedBlob ->[]

    let findInstance mb upath =
        mb.InstancePathList |> List.partition  (fun x->x.Path = upath)

    let findLink mb upath =
        mb.LinkPathList |> List.partition (peEqual upath)

let hashRootStr (repo:Repo) =
    Path.Combine(repo.Path.FullName, ".uit", "hash")

let removeTxtExt (name:string) = trimEnd ".txt" name

let listHash repo =
    let dir2hash (di:DirectoryInfo) =        
        let bs = di.Name
        listFiles di
        |> List.filter (fun fi->fi.Name.EndsWith(".txt"))
        |> List.map (fun fi->(removeTxtExt fi.Name))
        |> List.map (fun rest-> bs+rest)
        |> List.map (string2bytes >> Hash)

    DirectoryInfo(hashRootStr(repo)).EnumerateDirectories()
    |> Seq.map dir2hash
    |> Seq.concat
    |> Seq.toList

let listMB repo =
    listHash repo
    |> List.map (Blob.fromHash repo)
    |> List.map (fun bi -> match bi with |ManagedBlob mb->mb|UnmanagedBlob -> failwith("never reached"))


let listDupMB repo =
    listMB repo
    |> List.filter (fun mb-> match mb.InstancePathList with |_::_::_->true|_->false )

/// 文字列に一致するハッシュの一覧
let listHashWith repo (hashstr:string) =
    if hashstr.Length < 2 then
        []
    else
        let dirname = hashstr.[0..1]
        let dirRoot = hashRootStr repo
        let dir = DirectoryInfo(Path.Combine(dirRoot, dirname))
        if not dir.Exists then
            []
        else
            listFiles dir
            |> List.filter (fun fi -> fi.Name.StartsWith(hashstr.[2..]))
            |> List.map (fun fi-> dirname + (removeTxtExt fi.Name))
            |> List.map (string2bytes >> Hash)


