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

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Recursive -> "Recursively init to sub folders."

and CliArguments =
    | [<CliPrefix(CliPrefix.None)>] Init of ParseResults<InitArgs>
    | [<CliPrefix(CliPrefix.None)>] InitAt of path:string
    | [<CliPrefix(CliPrefix.None)>] Ingest of child:string
    | [<CliPrefix(CliPrefix.None)>] Lsdup
    | [<CliPrefix(CliPrefix.None)>] Uniqit of hash:string
    | [<CliPrefix(CliPrefix.None)>] Rm of path:string
    | [<CliPrefix(CliPrefix.None)>] Ls of path:string // lsfi and DInfo.ls
    | [<CliPrefix(CliPrefix.None)>] Lsh of hash:string
    | [<CliPrefix(CliPrefix.None)>] Mv of src:string * dest:string // moveDir
    | [<CliPrefix(CliPrefix.None)>] Cp of src:string * dest:string // copyDir
    | [<CliPrefix(CliPrefix.None)>] Toi of path:string
    | [<CliPrefix(CliPrefix.None)>] Import of path:string // importFile, importDir

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Init _ -> "Initialize current directory tree as uit managed repository."
            | InitAt _ -> "Init at specified directory and subdirectory as uit managed repository.\n.uit directory is createt at specify directory and not managed in current directory's .uit.\nAlways recursive."
            | Ingest _ -> "Ingest child directory managed by uit.\n .uit directory is merged to parent."
            | Lsdup _ -> "List duplicate instanced files."
            | Uniqit _ -> "Uniqify instanced file matched by hash pattern, then make all other files to link files."
            | Rm _ -> "Remove file"
            | Ls _ -> "Show file info." // lsfi and DInfo.ls
            | Lsh _ -> "Show hash info. Partially matched."
            | Mv _ -> "Move file or dir" // moveDir
            | Cp _ -> "Copy file or dir" // copyDir
            | Toi _ -> "ToInstance file"
            | Import _ -> "Import dir or file" // importFile, importDir

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArguments>(programName = "uit")
    try
        let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        if (results.Contains Init) then
            let initarg = results.GetResult(Init)
            let isrec = initarg.Contains Recursive
            let repo = {Path = DirectoryInfo "." }            

            if isrec then
                initRecursive repo
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
            let targetDI = results.GetResult(Ingest) |> DirectoryInfo
            let target = UDir.fromDI repo targetDI
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
            let upath = results.GetResult(Ls) |> FileInfo |> UPath.fromFileInfo repo
            match DInfo.findFInfo repo upath with
            | Some finfo ->
                 dispFInfo finfo
                 0
            | None ->
                 printfn "File not found."
                 1
        elif (results.Contains Lsh) then
            let repo = currentRepo ()
            let hashpat = results.GetResult(Lsh)
            lshmb repo hashpat
            |> dispMb
            0
        else
            printfn "NYI"
            1
    with e ->
        printfn "%s" e.Message
        1
