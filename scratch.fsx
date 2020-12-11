
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
