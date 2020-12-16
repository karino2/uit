module FInfo

open Common
open System
open System.IO

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

type DirInfo = Repo -> UDir -> FInfo list
type ToFInfo = Repo -> UPath -> FInfo option

type FInfos2Text = FInfo list -> string

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

let fromUPath :ToFInfo = fun repo upath ->
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


let updateDirInfo repo udir newFi =
    let rest =
        dirInfo repo udir
        |> List.filter (fun finfo-> finfo.Hash <> newFi.Hash)
    let newFis = newFi::rest
    saveDirFInfos repo udir newFis
    newFis


