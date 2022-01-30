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
type FInfoT = {
    Entry : Entry
    Hash: Hash
    FName: string
}

module FInfo =
    let computeFrom fi =
        let hash = computeFileHash fi
        {Hash = hash; FName=fi.Name; Entry={Type=Instance; LastModified=fi.LastWriteTime; EntryDate=DateTime.Now}}

    let toText finfo =
        let fmt tp (lastmod:DateTime) (entdt:DateTime) hash fname =
            sprintf "%d\t%d\t%d\t%s\t%s" tp lastmod.Ticks entdt.Ticks (hash2string hash) fname
        let format tp finfo =
            fmt tp finfo.Entry.LastModified finfo.Entry.EntryDate finfo.Hash finfo.FName
        match finfo.Entry.Type with
        | Instance -> format 1 finfo
        | Link -> format 2 finfo
    
/// DInfoT はFInfo list。今のところわざわざ型を定義しなくても良いかと思いFInfo listをそのまま使っている。
module DInfo =
    let toText finfos =
        finfos
        |> List.map FInfo.toText
        |> String.concat "\n"

    let computeFrom repo udir =
        let di = UDir.toDI repo udir
        listFiles di
        |> List.map FInfo.computeFrom

    let dirRootStr (repo:Repo) =
        Path.Combine( repo.Path.FullName, ".uit", "dirs")

    let dirFI repo udir =
        let dirRoot = dirRootStr repo
        let relative = UDir.toOSPath udir
        let dir = 
            if String.IsNullOrEmpty relative then
                dirRoot
            else
                Path.Combine(dirRoot, relative)
        Path.Combine(dir, "dir.txt") |> FileInfo

    /// udirのdir.txtを読んでDInfoを返す
    let ls repo udir =
        let fi = dirFI repo udir
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
        if fi.Exists then
            File.ReadLines(fi.FullName)
            |> Seq.map toFInfo
            |> Seq.toList
        else
            []
    
    let rec enumerateDirTxtUnder (di:DirectoryInfo) =
        let fs = di.EnumerateFiles() |> Seq.filter (fun fi -> fi.Name = "dir.txt" )
        di.EnumerateDirectories()
        |> Seq.map enumerateDirTxtUnder
        |> Seq.concat
        |> Seq.append fs


    /// repo内のdirs下のすべてのdir.txtのFileInfoを返す。
    let enumerateDirTxtFI repo =
        dirRootStr repo |> DirectoryInfo
        |> enumerateDirTxtUnder

    /// repo内のdirs下のすべてのdir.txtに関する以下のタプルを返す
    /// (相対パス, FileInfo)
    /// 相対パスは以下の２つの場合がある（一貫性がいまいち）
    /// - ルート ""
    /// - 子供 "/folder3" など
    let enumerateDirTxt repo = 
        let rootdi = dirRootStr repo |> DirectoryInfo
        enumerateDirTxtUnder rootdi
        |> Seq.map (fun fi-> (fi.Directory.FullName.Substring(rootdi.FullName.Length), fi))    


    let save repo udir fis =
        let dirfi = dirFI repo udir
        dirfi.Directory |> ensureDir
        toText fis
        |> saveText dirfi

    // FInfoのリストを求めてそれを保存
    let computeAndSave repo udir=
        let fis = computeFrom repo udir
        save repo udir fis
        fis

    /// FInfoを探すのにDInfoモジュールにあるのは不自然に見えるかもしれないが、
    /// DInfo.lsから探すので依存関係的にはここでないといけない。
    /// dirs.txtから探す事を思えばこちらで良いのだが、少しわかりにくい。
    let findFInfo repo upath =    
        let udir = parentDir upath
        let fname = UPath.fileName upath
        let eqname name (fi:FInfoT) =
            name = fi.FName
        let fis = ls repo udir

        let found = fis |> List.filter (eqname fname)
        match found with
        | x::_ -> Some x
        | _-> 
            let found2 = fis |> List.filter (eqname (fname + LinkExt))
            match found2 with
            | y::_ -> Some y
            | _-> None

    let updateFInfo repo udir orgname newFi =
        let rest =
            ls repo udir
            |> List.filter (fun finfo-> finfo.FName <> orgname)
        let newFis = newFi::rest
        save repo udir newFis
        newFis

    let removeFileEntry repo upath =
        let udir = parentDir upath
        let fname = UPath.fileName upath
        let newFis =
            ls repo udir
            |> List.filter (fun finfo-> finfo.FName <> fname)
        save repo udir newFis
        newFis


