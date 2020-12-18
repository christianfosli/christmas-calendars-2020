#r "nuget: FParsec"

open System
open System.IO
open FParsec
open FParsec.Primitives
open FParsec.CharParsers

module Expression =
    type T =
        | Number of int64
        | Add of T * T
        | Multiply of T * T

    let rec evaluate expression =
        match expression with
        | Number n -> n
        | Add (x, y) -> evaluate x + evaluate y
        | Multiply (x, y) -> evaluate x * evaluate y

    let private opp, expr =
        // ref https://www.quanttec.com/fparsec/reference/operatorprecedenceparser.html#members.OperatorPrecedenceParser
        // and a little https://github.com/tormodfj/aoc2020/blob/main/18-1/Program.fs :-)
        let ws = spaces
        let str_ws s = pstring s >>. ws

        let opp =
            OperatorPrecedenceParser<T, unit, unit>()

        let expr = opp.ExpressionParser

        let term =
            (pint64 .>> ws |>> Number)
            <|> between (str_ws "(") (str_ws ")") expr

        opp.TermParser <- term

        opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, (fun x y -> Add(x, y))))
        opp.AddOperator(InfixOperator("*", ws, 1, Associativity.Left, (fun x y -> Multiply(x, y))))

        opp, expr

    let ofString (str: string) =
        match run expr str with
        | Success (ex, _, _) -> Some ex
        | Failure _ -> None

File.ReadLines "input.txt"
|> Seq.map Expression.ofString
|> Seq.choose id
|> Seq.map Expression.evaluate
|> Seq.sum
|> printfn "Sum of values: %i"

// Tests
open Expression

Add(Multiply(Number 2L, Number 3L), Multiply(Number 4L, Number 5L))
|> evaluate
|> fun ans ->
    match ans with
    | 26L -> ()
    | err -> failwithf "Expected 26 but got %i" err

Expression.ofString "2 * 3 + (4*5)"
|> Option.map evaluate
|> fun ans ->
    match ans with
    | Some x when x = 26L -> ()
    | err -> failwithf "Expected 26 but got %A" err
