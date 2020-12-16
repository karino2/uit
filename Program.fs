module Program

open System


[<EntryPoint>]
let main argv =
    // Define a function to construct a message to print
    let from whom =
        sprintf "from %s" whom

    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code