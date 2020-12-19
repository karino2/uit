module InteractiveUtils

open Common
open Blob
open FInfo
open System.IO

// fsharplint:disable Hints
let deleteUitDir (repo:Repo) =
    Directory.Delete(Path.Combine(repo.Path.FullName, ".uit") ,true)

let mb2upaths mb =
    let ip =
        mb.InstancePathList
        |> List.map (fun pe-> pe.Path)
    let rp =
        mb.LinkPathList
        |> List.map (fun pe-> pe.Path)
    List.append ip rp

let dispMb (mb:ManagedBlob) =
    printfn "Hash: %s" (hash2string mb.Hash)
    printf "Inst: "
    mb.InstancePathList |> List.iter (fun pe->printf "%A " pe.Path)
    printf "\nLinks: "
    mb.LinkPathList |> List.iter (fun pe->printf "%A " pe.Path)
    printfn ""

let lsa repo upath =
    let opbinfo = FInfo.fromUPath repo upath
                |> Option.map (fun fi->fi.Hash)
                |> Option.map (fromHash repo)
    match opbinfo with
    | (Some (ManagedBlob mb)) -> dispMb mb
    | _ -> ()

let lsmb repo upath =
    let opbinfo = FInfo.fromUPath repo upath
                |> Option.map (fun fi->fi.Hash)
                |> Option.map (fromHash repo)
    match opbinfo with
    | (Some (ManagedBlob mb)) -> mb
    | _ -> failwith("not managed path")

let lshmb repo hashpat = 
    let hash = listHashWith repo hashpat
    match hash with
        |[one] -> 
            match (fromHash repo one) with
            | ManagedBlob mb -> mb
            | UnmanagedBlob -> failwith "unmanaged blob, maybe corrupted."
        |_ -> 
            let msg = sprintf "matched hash: %A" hash
            failwith(msg)
