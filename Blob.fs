module Blob

open Common
open System
open System.IO

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

type FromHash = Repo -> Hash -> BlobInfo
type PathToBInfo = Repo -> UPath -> BlobInfo

type ListHash = Repo -> Hash list
// 文字列に一致するハッシュの一覧
type ListHashWith = Repo -> string -> Hash list
type ListMF = Repo -> ManagedBlob list
type ListDupMF = Repo -> ManagedBlob list

// いる？
type Paths = Repo -> Hash -> UPath list


type SaveBInfo = Repo -> ManagedBlob -> unit

type InstancePaths = ManagedBlob -> PathEntry list
type LinkPaths = ManagedBlob -> PathEntry list

type UPath2BInfo  = Repo -> UPath -> BlobInfo option

type ToText = ManagedBlob -> string


let mb2text :ToText = fun mb->
    let totext tp (pe: PathEntry) =
        let (UPath path) = pe.Path
        sprintf "%d\t%d\t%d\t%s\n"  tp pe.Entry.LastModified.Ticks pe.Entry.EntryDate.Ticks path
    let instances = mb.InstancePathList |> List.map (totext 1)
    let links = mb.LinkPathList |> List.map (totext 2)
    List.append instances links |> List.reduce (+)

let fromHash :FromHash = fun repo hash ->
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

let saveMf repo (mb:ManagedBlob) =
    let dest = hashPath mb.Hash
    saveText (toFileInfo repo dest) (mb2text mb)


let bi2instances bi =
    match bi with
    | ManagedBlob mb-> mb.InstancePathList
    | UnmanagedBlob ->[]

let bi2links bi =
    match bi with
    | ManagedBlob mb-> mb.LinkPathList
    | UnmanagedBlob ->[]


// using?
let pe2path pe = pe.Path

let bi2allpes bi =
    List.append (bi2instances bi) (bi2links bi)


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
    |> List.map (fromHash repo)
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


let findInstance mb upath =
    mb.InstancePathList |> List.partition  (fun x->x.Path = upath)

let partitionPE target pelist =
    List.partition (peEqual target) pelist
