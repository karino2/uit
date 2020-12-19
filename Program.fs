module Program

open System
open Common
open InteractiveUtils
open System.IO

[<EntryPoint>]
let main argv =

    let repo = { Path = DirectoryInfo "/Users/arinokazuma/work/testdata" }
    let mb = lsmb repo (UPath.fromUit "sns/text/test.cpp")
    dispMb mb
    
    0 // return an integer exit code