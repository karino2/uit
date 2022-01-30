module Program

open System
open Common
open InteractiveUtils
open System.IO
open Argu
open CommandLine

// lsfi, lsmb, lsa, DInfo.ls


type CliArguments =
    | [<CliPrefix(CliPrefix.None)>] Init
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
            | Init _ -> "Init"
            | Lsdup _ -> "Lsdup"
            | Uniqit _ -> "UniqIt"
            | Rm _ -> "Remove file"
            | Ls _ -> "ls file" // lsfi and DInfo.ls
            | Lsh _ -> "lsh hash"
            | Mv _ -> "Move file or dir" // moveDir
            | Cp _ -> "Copy file or dir" // copyDir
            | Toi _ -> "ToInstance file"
            | Import _ -> "Import dir or file" // importFile, importDir

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArguments>(programName = "uit")
    let usage = parser.PrintUsage()
    printf "%s" usage
    let repo = { Path = DirectoryInfo "./testdata_work" }
    let mb = lsmb repo (UPath.fromUit "test1.txt")
    dispMb mb
    
    0 // return an integer exit code