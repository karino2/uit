module Program

open System
open Common
open InteractiveUtils
open System.IO

[<EntryPoint>]
let main argv =

    let repo = { Path = DirectoryInfo "/Users/arinokazuma/work/testdata" }
    let mb = lsmb repo (UPath.fromUit "imgs/美子ちゃん.pxv")
    dispMb mb
    
    0 // return an integer exit code