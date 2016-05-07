// F# Algebrai kifejezés megoldása Lengyel Formára hozással
// S9VX30
open System.IO
open System

type Token =
    | Number of double
    | Add
    | Substract
    | Multiply
    | Divide
    | ParenthesisOpen
    | ParenthesisClose

type PreToken =
    | Number of int
    | Parenthesis
    | Add
    | Substract
    | Multiply
    | Divide
    | Dot
    | ParenthesisOpen
    | ParenthesisClose
    | ErrorOrEmpty

let preTokenizeChar (input:char) =
    match input with
    | '1' -> Number 1
    | '2' -> Number 2
    | '3' -> Number 3
    | '4' -> Number 4
    | '5' -> Number 5
    | '6' -> Number 6
    | '7' -> Number 7
    | '8' -> Number 8
    | '9' -> Number 9
    | '0' -> Number 0
    | '+' -> Add
    | '-' -> Substract
    | '*' -> Multiply
    | '/' | ':' -> Divide
    | '.' | ',' -> Dot
    | _ -> ErrorOrEmpty

let preTokenize (input:char[]) =
    input |> Seq.map preTokenizeChar

let tokenize (input:char[]) =
    let preTokens = preTokenize input
    // Itt valahogy végigmegyek a pre-tokeneken és ha szám van benne, összerakom
    // egy nagy számmá, Dot-ot is beleértve
    // let tokens = Seq.fold

    [|1|]

[<EntryPoint>]
let main argv = 

    let expression = File.ReadAllText("expression.txt");
    printfn "Beolvasott kifejezés: %s" expression

    let tokens = tokenize (expression.ToCharArray())

    0 // return an integer exit code

