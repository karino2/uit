module CommandLine
open System.IO
// fsharplint:disable Hints


/// .uitディレクトリがあるディレクトリを親に向かって探していく。
/// 複数ある場合は一番上のモノを返す。
/// 無ければNone
let findTopUitDir (startDI: DirectoryInfo) =
    let rec findUitDir cand (nextDI:DirectoryInfo) =
        match nextDI with
        | null -> cand
        | _ ->
            match nextDI.EnumerateDirectories(".uit") |> Seq.toList with
            | [] -> findUitDir cand nextDI.Parent
            | _ -> findUitDir (Some nextDI) nextDI.Parent
    findUitDir None startDI

