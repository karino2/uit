module TestUtils

open System
open Blob
open System.Diagnostics

type UitException(message) = inherit Exception(message)

let should left right =
    if left <> right then
        raise (UitException (sprintf "Not equal, left=(%A), right=(%A)" left right))

let shouldNot left right =
    if left = right then
        raise (UitException (sprintf "Wrongly Equal, left=(%A), right=(%A)" left right))

let shouldSome a =
    match a with
    | Some _ -> ()
    | None -> raise (UitException "Wrongly None")

let shouldNone a =
    match a with
    | Some value -> raise (UitException (sprintf "Wrongly Some: %A" value))
    | None -> ()

let instLen mb =
    List.length mb.InstancePathList

let linkLen mb =
    List.length mb.LinkPathList

let shellExecute cmdname args=
    use proc = new Process()
    proc.StartInfo.FileName <- cmdname
    proc.StartInfo.Arguments <- args
    proc.StartInfo.RedirectStandardOutput <- true
    proc.Start() |> ignore
    let ret = proc.StandardOutput.ReadToEnd()
    proc.WaitForExit()
    ret
