// F# Algebrai kifejezés megoldása Lengyel Formára hozással
// S9VX30
module Main

open System.IO
open System
open System.Collections.Generic

// TODO debug printfn-eket kiszedni
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

// Neten talált példa számok összevonására ha megegyeznek
//let group (p:'a -> 'a -> bool) (s:'a seq) = 
//    if s |> Seq.isEmpty then seq []
//    else
//        let h = s |> Seq.head
//        let state = ( (h,seq [h]) , Seq.empty)
//        let ((a,b),c) = 
//            s |> Seq.skip 1 
//            |> Seq.fold (fun ( (a,b),c) i -> 
//                            if p a i then ( (a,seq { yield! b; yield i}) ,c)
//                            else ( (i, seq {yield i}), seq {yield! c; yield b}) ) 
//                            state 
//        seq { yield! c; yield b}
// Net példa vége

let isOperator input =
    match input with
    | Token.Number x -> false
    | Token.ParenthesisOpen | Token.ParenthesisClose -> false
    | _ -> true

let precedence input =
    match input with
    | Token.Add | Token.Substract -> 1
    | Token.Multiply | Token.Divide -> 2
    | _ -> 0

// Pretokeneket tokenné alakítja
let tokenize (input:char[]) =

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
        | '(' -> ParenthesisOpen
        | ')' -> ParenthesisClose
        | _ -> ErrorOrEmpty

    // Minden karaktert preTokenre konvertál
    let preTokens = input |> Seq.map preTokenizeChar

    // TODO
    // Itt valahogy végigmegyek a pre-tokeneken és ha szám van benne, összerakom
    // egy nagy számmá, Dot-ot is beleértve
    // let tokens = Seq.fold

  
    //  let ra : seq<Token> = Seq.empty

//    let result = preTokens |> Seq.choose( fun i-> 
//        match i with
//        | Number x -> Some(Number x)
//        | Add -> Some(Add)
//        | _ -> None
//    )

    let cleanPreTokens = preTokens |> Seq.filter(fun x -> 
        match x with
        | ErrorOrEmpty -> false
        | _ -> true
        )

    // Olyan függvényt kellene kitalálni, ami összevonja az egymás után
    // jövő number és dot type-okat egy számmá
    let processedTokens : seq<Token> = cleanPreTokens |> Seq.map(fun current ->

        let mutable num = ""  
                      
        // Ha szám, akkor a kis num-ban kellene gyűjteni és kellene tudni, 
        // hogy a következő szám-e. Ha igen, ki kell rakni a num-ból 
        // tokenbe a teljes számot
        // TODO

        // ha más nem lesz végigmegyek forral és vizsgálom az i+1-ik elemet, eltárolom 
        // honnan volt csak szám (pl.: ha az i=4. elemtől szám, addig keresek amíg az i+1 
        // nem szám és átadom a tárolt 4-től i-ig egy parse függvénynek), közben egy mutable 
        // másik array-be pakolgatom be az eredményt, így tudna változni a hossza.

        // Megoldás lehet még az is, hogy karakterről karakterre, pretoken nélkül parse-olok
        // és ha ismert, egy karakteres operátort találok, kiteszem, ha nem, összegyűjtöm egy
        // (stack-re :) és a következő ismert egykarakteres operátornál az összegyűjtött 
        // karakterláncot trim-elve próbálom értelmezni, így a 14.4 cos stb. kifejezések
        // itt értelmezhetőek lennének

        let token : Token = 
            match current with
            | Add -> Token.Add
            | Substract -> Token.Substract
            | Multiply -> Token.Multiply
            | Divide -> Token.Divide
            | ParenthesisOpen -> Token.ParenthesisOpen
            | ParenthesisClose -> Token.ParenthesisClose
            | _ -> Empty
          
        // És olyan seq függvény kellene amiből ki tudok hagyni elemeket, bár ez nem feltétlen szükséges
        token        
        )

    // Ezzel térünk vissza
    // TODO elég lesz a processedTokens, az empty-ket kiszedjük később
//    processedTokens |> Seq.filter(fun x -> 
//        match x with
//        | Empty -> false
//        | _ -> true
//        )

    seq [Token.Number 5.0; Token.Add; Token.Number 12.2; Token.Substract; Token.ParenthesisOpen; 
        Token.Number 4.0; Token.Multiply; Token.Number 3.0; Token.ParenthesisClose; Token.Divide; Token.Number 3.0; 
        Token.Add; Token.ParenthesisOpen; Token.Number 5.0; Token.Add; Token.Number 2.0; Token.ParenthesisClose; Token.Multiply; Token.Number 4.0]

// Tokenekkben leírt infix kifejezés postfix kifejezéssé konvertálása
let convertToPolishNotation inputTokens =
    let mutable stack = new Stack<Token>()
    let mutable postfix = new List<Token>()

    for token in inputTokens do
        match token with
        | Token.Empty -> ()
        | Token.ParenthesisOpen -> 
            printfn "--push %A" token
            stack.Push Token.ParenthesisOpen
        | Token.ParenthesisClose -> 
            // A nyitó zárójelig mindent felvenni a PF-be a veremből
            printfn "--ParenthesisClose, start popping from: %A" stack
            while (stack.Peek() <> Token.ParenthesisOpen ) do
                let popped = stack.Pop()
                printfn "--pop %A, addp [1]" popped
                postfix.Add(popped)
            
            stack.Pop() |> ignore

        | Token.Number x -> 
            printfn "--addp %A [2]" token
            postfix.Add (Token.Number x)
        | _ -> 
            // Műveleti jel: ha van nagyobb precedenciájú operátor, akkor kivenni, utána operátor verembe
            // If token is an operator (x) [S3]:
            // While there is an operator (y) at the top of the operators stack and either (x) is
            // left-associative and its precedence is less or equal to that of (y)
            // Pop (y) from the stack;
            // Add (y) output buffer;
            // Push (x) on the stack;
            if (stack.Count = 0) then
                printfn "--push %A (empty stack) [3]" token
                stack.Push(token)
            else
                printfn "--operator, check stack: %A" stack
                let rec check op =
                    if (( op |> isOperator ) && ((op |> precedence) >= (token |> precedence))) then
                        let popped = stack.Pop()
                        postfix.Add(popped)
                        printfn "--pop %A, addp [4]" popped
                        if (stack.Count > 0) then
                            stack.Peek() |> check

                stack.Peek() |> check 
                printfn "--push %A [5]" token
                stack.Push(token)

    // A veremből mindent kipakolni a PF-be
    while stack.Count > 0 do
        stack.Pop() |> postfix.Add 

    postfix

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

[<EntryPoint>]
let main argv = 

    let expression = File.ReadAllText("expression.txt");
    printfn "Beolvasott kifejezés: %s" expression

    let tokens = tokenize (expression.ToCharArray())

    printfn "Tokenek: %A" (Seq.toList tokens)

    let polishTokens = tokens |> convertToPolishNotation


    printfn "Ezt várjuk: 5 12.2 + 4 3 * 3 / - 5 2 + 4 * + "
    printfn "postfix-tokenek: %A" (Seq.toList polishTokens)

    printfn "Ezt várjuk: 41.2"
    let solution = polishTokens |> solvePolishNotation
    printfn "megoldas: %A" solution

    // TODO: A végén ha nem kell debug az egészet összepipe-ozni

    0 // return an integer exit code