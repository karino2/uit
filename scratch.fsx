[0..5]

open System.IO
Path.Combine(".uit", "trash")

type TraceBuilder() =
    member this.Delay(funcToDelay) =
        let delayed = fun()->
            printfn "%A - starting delayed fn." funcToDelay
            let res = funcToDelay()
            printfn "%A - Finished Delayed. result=%A" funcToDelay res
            res
        printfn "%A - Delaying using %A" funcToDelay delayed
        delayed

    member this.Return(x) = 
        printfn "Return an unwrapped %A as an option" x
        Some x

    member this.Zero() = 
        printfn "Zero"
        None

    member this.Combine (a,b) = 
        printfn "Returning early with %A. Ignoring second part: %A" a b 
        a

    member this.Run(funcToRun) =
        printfn "%A - Run Start." funcToRun
        let res = funcToRun()
        printfn "%A - Run End. Result is %A" funcToRun res
        res
        
   
let trace = TraceBuilder()

trace {
    printfn "Part 1: about to return 1"
    return 1
    printfn "Part 2: after return has happened"
    } |> printfn "Result for Part1 without Part2: %A"

let f = trace {
    printfn "Part 1: about to return 1"
    return 1
    printfn "Part 2: after return has happened"
    }

f() |> printfn "Result for Part1 without Part2: %A"

type ListBuilder() =
    member this.Bind(m, f) =
        m |> List.collect f

    member this.Zero() =
        printfn "Zero"
        []

    member this.Yield(x) =
        printfn "Yield %A as a list" x
        [x]
    
    member this.YieldFrom(m) =
        printfn "Yield %A directly" m
        m

    member this.For(m, f) =
        printfn "For %A" m
        this.Bind(m, f)
    
    member this.Combine(a, b) =
        printfn "combining %A and %A" a b
        List.concat [a; b]

    member this.Delay(f) =
        printfn "Delay"
        f()

let listbuilder = ListBuilder()

listbuilder {
    yield 1
    yield 2
} |> printfn "Result :%A "

listbuilder {
    for i in ["red"; "blue"] do
        yield i
        for j in ["hat"; "tie"] do
            yield! [i + " " + j; "-"]
} |> printfn "Result :%A "

listbuilder {
    yield 1
    yield 2
    yield 3
    yield 4
} |> printfn "Result :%A "



let rec loopAndPrint aList = 
    match aList with 
    // empty list means we're done.
    | [] -> 
        printfn "empty" 

    // binding to head::tail. 
    | x::xs -> 
        printfn "element=%A," x
        // do all over again with the 
        // rest of the list
        loopAndPrint xs 


loopAndPrint [1..5]

// format
let rows = [ (1,"a"); (-22,"bb"); (333,"ccc"); (-4444,"dddd") ] 

// no alignment
for (i,s) in rows do
    printfn "|%i|%s|" i s

// with alignment
for (i,s) in rows do
    printfn "|%5i|%5s|" i s

// with left alignment for column 2
for (i,s) in rows do
    printfn "|%5i|%-5s|" i s

// with dynamic column width=20 for column 1
for (i,s) in rows do
    printfn "|%*i|%-5s|" 20 i s 

// with dynamic column width for column 1 and column 2
for (i,s) in rows do
    printfn "|%*i|%-*s|" 20 i 10 s


// stack calc

type Stack = StackContents of float list
let push x (StackContents content) =
    StackContents (x::content)

let EMPTY = StackContents []

let ONE = push 1.0
let TWO = push 2.0
let THREE = push 3.0

let pop (StackContents contents) =
    match contents with
    | top::rest ->
        (top, StackContents rest)
    | [] -> failwith "Stack underflow"


let binary binFn stack =
    let x, s = pop stack
    let y, ss = pop s
    push (binFn x y) ss


let ADD stack =
    binary (+) stack

let SUB stack =
    binary (-) stack

// pig latin

let vowels = set ['a';'e';'i';'o';'u']

let toPigLatin (word: string) =
    let isVowel (c: char) =
        match c with
        | 'a' | 'e' | 'i' |'o' |'u'
        | 'A' | 'E' | 'I' | 'O' | 'U' -> true
        |_ -> false
    
    if isVowel word.[0] then
        word + "yay"
    else
        word.[1..] + string(word.[0]) + "ay"
   
toPigLatin "Kazuma"
toPigLatin "Arino"

let evalWith5ThenAdd2 fn = fn 5 + 2

let add1 x = x + 1

evalWith5ThenAdd2 add1

let five = 5

let addGenerator numberToAdd = (+) numberToAdd

let add5 = addGenerator 5

add5 7

let something =
    2+2 |> ignore
    3+7
