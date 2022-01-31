module Program

open System
open Common
open InteractiveUtils
open System.IO
open Action
open Argu
open CommandLine
open Blob
open FInfo

// lsfi, lsmb, lsa, DInfo.ls


[<CliPrefix(CliPrefix.None)>] 
type InitArgs =
    |[<AltCommandLineAttribute("-r")>] Recursive
    |[<AltCommandLineAttribute("-s")>] Silent

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Recursive -> "Recursively init to sub folders."
            | Silent -> "Suppress logging."
and LsmArgs =
    |[<AltCommandLineAttribute("-d")>] DirOnly
    |[<Mandatory; MainCommand; ExactlyOnce; Last>] Path of path:string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Path _ -> "Target path."
            | DirOnly -> "Show directory only."

and CliArguments =
    | [<CliPrefix(CliPrefix.None)>] Init of ParseResults<InitArgs>
    | [<CliPrefix(CliPrefix.None)>] InitAt of path:string
    | [<CliPrefix(CliPrefix.None)>] Ingest of child:string
    | [<CliPrefix(CliPrefix.None)>] Lsdup
    | [<CliPrefix(CliPrefix.None)>] Uniqit of hash:string
    | [<CliPrefix(CliPrefix.None)>] Rm of path:string
    | [<CliPrefix(CliPrefix.None)>] Ls of path:string // lsfi and DInfo.ls
    | [<CliPrefix(CliPrefix.None)>] Lsm of ParseResults<LsmArgs> // list managed
    | [<CliPrefix(CliPrefix.None)>] Lsh of hash:string
    | [<CliPrefix(CliPrefix.None)>] Add of path:string // Add one unmanaged file to managed.
    | [<CliPrefix(CliPrefix.None)>] Mv of src:string * dest:string // moveDir
    | [<CliPrefix(CliPrefix.None)>] Cp of src:string * dest:string // copyDir
    | [<CliPrefix(CliPrefix.None)>] Inst of path:string
    | [<CliPrefix(CliPrefix.None)>] Root
    | [<CliPrefix(CliPrefix.None)>] Import of src:string * dest:string // importFile, importDir

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Init _ -> "Initialize current directory tree as uit managed repository."
            | InitAt _ -> "Init at specified directory and subdirectory as uit managed repository.\n.uit directory is createt at specify directory and not managed in current directory's .uit.\nAlways recursive."
            | Ingest _ -> "Ingest child directory managed by uit.\n .uit directory is merged to parent."
            | Lsdup _ -> "List duplicate instanced files."
            | Uniqit _ -> "Uniqify instanced file matched by hash pattern, then make all other files to link files."
            | Rm _ -> "Remove file. If there is no other hash file, move to trash. File data must exist somewhere."
            | Ls _ -> "Show file info." // lsfi and DInfo.ls
            | Lsm _ -> "List managed file and dirs."
            | Lsh _ -> "Show hash info. Partially matched."
            | Add _ -> "Add one unmanaged file to managed."
            | Mv _ -> "Move file or dir" // moveDir
            | Cp _ -> "Copy file or dir" // copyDir
            | Inst _ -> "To instance file. If there is another instance, make that file to link and move that file instance to target."
            | Root -> "Show current .uit path."
            | Import _ -> "Import outer unmanaged dir or file" // importFile, importDir



[<EntryPoint>]
let main argv =

    let toUPath repo file = 
        FileInfo file |> UPath.fromFileInfo repo

    let toUDir repo dirstr =
        let di = DirectoryInfo dirstr
        if not di.Exists then
            sprintf "Directory %s not exists" dirstr
            |> failwith
        UDir.fromDI repo di

    let toUDirPair repo (src, dest) =
        let srcudir = toUDir repo src
        let destudir = toUDir repo dest 
        (srcudir, destudir)

    let importFileOrDir repo (src, dest) =
        let srcdi = DirectoryInfo src
        if srcdi.Exists then
            // directory import
            // dest はまだ存在しないのでtoUDirは使えない
            let destudi = DirectoryInfo dest |> UDir.fromDI repo
            Import.importDir repo srcdi destudi
            0
        else
            // file import
            let fi = FileInfo src
            if not fi.Exists then
                eprintfn "src %s not exists." src
                1
            else
                let destupath = toUPath repo dest
                Import.importFile repo fi destupath |> ignore
                0

    let parser = ArgumentParser.Create<CliArguments>(programName = "uit")
    try
        let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        if (results.Contains Init) then
            let initarg = results.GetResult(Init)
            let isrec = initarg.Contains Recursive
            let logger =
                if initarg.Contains Silent then
                    (fun str->())
                else
                    (fun str->printf "%s" str)
            let repo = {Path = DirectoryInfo "." }

            if isrec then
                initRecursiveWithLogger repo logger
            else
                init repo |> ignore
            0
        elif (results.Contains InitAt) then
            let target = results.GetResult(InitAt)
            let repo = {Path = DirectoryInfo target }            
            initRecursive repo
            0
        elif (results.Contains Ingest) then
            let repo = currentRepo ()
            let target = results.GetResult(Ingest) |> toUDir repo
            Ingest.ingest repo target
            0
        elif (results.Contains Lsdup) then
            let repo = currentRepo ()
            listDupMB repo |> List.iter dispMb 
            0
        elif (results.Contains Uniqit) then
            let repo = currentRepo ()
            let hashpat = results.GetResult(Uniqit)
            lshmb repo hashpat
            |> uniqIt repo 
            |> ignore
            0
        elif (results.Contains Ls) then
            let repo = currentRepo ()
            let upath = results.GetResult(Ls) |> toUPath repo
            match DInfo.findFInfo repo upath with
            | Some finfo ->
                 dispFInfo finfo
                 0
            | None ->
                 eprintfn "File not managed."
                 1
        elif (results.Contains Lsm) then
            let repo = currentRepo ()
            let lsmargs = results.GetResult(Lsm)
            let udir = lsmargs.GetResult(Path) |> toUDir repo            
            let isdironly = lsmargs.Contains DirOnly

            let dinfoDI = DInfo.dinfoDirPath repo udir |> DirectoryInfo

            if not isdironly then
                DInfo.ls repo udir
                |> List.iter (fun finfo-> printfn "%s" finfo.FName) 
            0

        elif (results.Contains Lsh) then
            let repo = currentRepo ()
            let hashpat = results.GetResult(Lsh)
            lshmb repo hashpat
            |> dispMb
            0
        elif (results.Contains Rm) then
            let repo = currentRepo ()
            let upath = results.GetResult(Rm) |> toUPath repo
            match DInfo.findFInfo repo upath with
            | Some finfo ->
                let mb = Blob.fromHashMB repo finfo.Hash
                Remove.remove repo mb upath |> ignore
                0
            | None ->
                eprintfn "File not managed."
                1
        elif (results.Contains Add) then
            let repo = currentRepo ()
            let fi = results.GetResult(Add) |> FileInfo
            let upath = UPath.fromFileInfo repo fi
            match DInfo.findFInfo repo upath with
            | Some _ ->            
                eprintfn "File already managed: %A" upath
                1
            | None ->
                let uparent = (parentDir upath)
                let orginfos = DInfo.ls repo uparent
                let newinfo = FInfo.computeFrom fi
                DInfo.save repo uparent (newinfo::orginfos)
                let mb = finfo2mb repo uparent newinfo
                Blob.save repo mb
                printfn "Added: %s" (UPath.toUitStr upath)
                0
        elif (results.Contains Mv) then
            let repo = currentRepo ()
            let (srcudir, destudir) = results.GetResult(Mv) |> toUDirPair repo
            moveDir repo srcudir destudir
            0
        elif (results.Contains Cp) then
            let repo = currentRepo ()
            let (srcudir, destudir) = results.GetResult(Cp) |> toUDirPair repo
            copyDir repo srcudir destudir
            0
        elif (results.Contains Inst) then
            let repo = currentRepo ()
            let upath = results.GetResult(Inst) |> toUPath repo
            match DInfo.findFInfo repo upath with
            | Some finfo ->
                let mb = Blob.fromHashMB repo finfo.Hash
                toInstance repo mb upath |> ignore
                0
            | None ->
                eprintfn "File not managed."
                1
        elif (results.Contains Root) then
            let diopt = findCurrentUitDir (DirectoryInfo ".")
            match diopt with
            | (Some di) ->
                printfn "%s" di.FullName 
                0
            | None -> 
                eprintfn ".uit not found."
                1
        elif (results.Contains Import) then
            let repo = currentRepo ()
            results.GetResult(Import)
            |> importFileOrDir repo 
        else
            printfn "NYI"
            1
    with e ->
        printfn "%s" e.Message
        1
