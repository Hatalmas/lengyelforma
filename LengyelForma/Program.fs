// F# Algebrai kifejezés megoldása Lengyel Formára hozással
// S9VX30
module Main

open System.IO
open System
open System.Collections.Generic

// TODO sorrendet felcserélni, hogy a token legyen láthatóbb és ne kelljen mindenhol prefixelni
type Token =
    | Number of double
    | Add
    | Substract
    | Multiply
    | Divide
    | ParenthesisOpen
    | ParenthesisClose
    | Empty

// Megadja, hogy a kapott token operátor-e
let isOperator input =
    match input with
    | Token.Number x -> false
    | Token.ParenthesisOpen | Token.ParenthesisClose -> false
    | _ -> true

// Visszaadja az operátorok precedenciáját az operátor stack kezeléséhez
let precedence input =
    match input with
    | Token.Add | Token.Substract -> 1
    | Token.Multiply | Token.Divide -> 2
    | _ -> 0

// Pretokeneket tokenné alakítja split segítségével
let tokenizeWithSplit (input:string) =
    let stretchedString = input.Replace("+", " + ").Replace("-", " - ").Replace(":", " : ").Replace("/", " : ").Replace(",", ".").Replace(")", " ) ").Replace("(", " ( ").Replace("*", " * ")

    let stringTokens = stretchedString.Split(' ')

    printfn "STOKENS: %A" stringTokens

    let identifyToken (inputToken:string) =
        match inputToken with
        | "+" -> Add
        | "-" -> Substract
        | "*" -> Multiply
        | ":" -> Divide
        | "(" -> ParenthesisOpen
        | ")" -> ParenthesisClose
        | ""  -> Empty
        | _ -> Number (inputToken |> Double.Parse)

    // Minden karaktert preTokenre konvertál
    stringTokens |> Seq.map identifyToken

// Tokenekkben leírt infix kifejezés postfix kifejezéssé konvertálása
let convertToPolishNotation inputTokens =
    let mutable stack = new Stack<Token>()
    let mutable postfix = new List<Token>()

    for token in inputTokens do
        match token with
        | Token.Empty -> ()
        | Token.ParenthesisOpen -> stack.Push Token.ParenthesisOpen
        | Token.ParenthesisClose -> 
            // A nyitó zárójelig mindent felvenni a PF-be a veremből
            while (stack.Peek() <> Token.ParenthesisOpen ) do
                let popped = stack.Pop()
                postfix.Add(popped)
            
            stack.Pop() |> ignore

        | Token.Number x ->  postfix.Add (Token.Number x)
        | _ -> 
            // Műveleti jel: ha van nagyobb precedenciájú operátor, akkor kivenni, utána operátor verembe
            // If token is an operator (x) [S3]:
            // While there is an operator (y) at the top of the operators stack and either (x) is
            // left-associative and its precedence is less or equal to that of (y)
            // Pop (y) from the stack;
            // Add (y) output buffer;
            // Push (x) on the stack;
            if (stack.Count = 0) then
                stack.Push(token)
            else
                let rec check op =
                    if (( op |> isOperator ) && ((op |> precedence) >= (token |> precedence))) then
                        let popped = stack.Pop()
                        postfix.Add(popped)
                        if (stack.Count > 0) then
                            stack.Peek() |> check

                stack.Peek() |> check 
                stack.Push(token)

    // A veremből mindent kipakolni a PF-be
    while stack.Count > 0 do
        stack.Pop() |> postfix.Add 

    postfix

// Végrehajtja az operatorban megadott műveletet az op1 és op2 operanduson
let applyOperator (operator, op1, op2) =
    match operator, op1, op2 with
        | Token.Add, Token.Number a, Token.Number b -> Token.Number (a + b)
        | Token.Substract, Token.Number a, Token.Number b -> Token.Number (b - a)
        | Token.Multiply, Token.Number a, Token.Number b -> Token.Number (a * b)
        | Token.Divide, Token.Number a, Token.Number b -> Token.Number (b / a)
        | _ -> failwith "Nem kezelt operátor vagy hibás stack"

// Postfix-kifejezés megoldása
let solvePolishNotation inputTokens =
    let mutable stack = new Stack<Token>()

    for token in inputTokens do
        match token with
        | Token.Number x -> stack.Push (Token.Number x)
        | _ -> stack.Push (applyOperator (token, stack.Pop(), stack.Pop()))

    match stack.Count with 
        | 1 -> stack.Pop()
        | _ -> failwith "Hibás megoldás stack"

// Belépési pont, expression.txt beolvasása és megoldása
[<EntryPoint>]
let main argv = 

    let expression = File.ReadAllText("expression.txt");
    printfn "Beolvasott kifejezés: %s" expression

//    let tokens = tokenize (expression.ToCharArray())
    let tokens = tokenizeWithSplit (expression)
    printfn "Tokenek: %A" (Seq.toList tokens)

    let polishTokens = tokens |> convertToPolishNotation
    printfn "Ezt várjuk: 5 12.2 + 4 3 * 3 / - 5 2 + 4 * + "
    printfn "postfix-tokenek: %A" (Seq.toList polishTokens)

    printfn "Ezt várjuk: 41.2"
    let solution = polishTokens |> solvePolishNotation
    printfn "megoldas: %A" solution

    // TODO: A végén ha nem kell debug az egészet összepipe-ozni

    0 // return an integer exit code