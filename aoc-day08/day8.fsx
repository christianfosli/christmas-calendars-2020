open System.IO

type Operation =
    | Accumulate
    | Jump
    | NoOp

type Instruction =
    { Operation: Operation
      Argument: int
      LineNumber: int }

let parse (index: int) (line: string) =
    let operation =
        match line.Substring(0, 3) with
        | "acc" -> Operation.Accumulate
        | "jmp" -> Operation.Jump
        | "nop" -> Operation.NoOp
        | _ -> failwith "Invalid Operation"

    let argument = line.Substring(4) |> int

    { Operation = operation
      Argument = argument
      LineNumber = index }

let rec run (code: Instruction list) (lineNumber: int) (history: Set<int>) (acc: int) (debug: bool) =
    let instruction = List.tryItem lineNumber code

    match instruction with
    | Some inst when Set.contains inst.LineNumber history ->
        if debug
        then printfn "Instruction %A executed earlier. Stopping execution with reg %i" inst acc

        None
    | Some inst ->
        match inst.Operation with
        | Accumulate -> run code (lineNumber + 1) (Set.add inst.LineNumber history) (acc + inst.Argument) debug
        | Jump -> run code (lineNumber + inst.Argument) (Set.add inst.LineNumber history) acc debug
        | NoOp -> run code (lineNumber + 1) (Set.add inst.LineNumber history) acc debug
    | None -> Some acc

let bootCode =
    File.ReadLines "input.txt"
    |> Seq.mapi parse
    |> List.ofSeq

// Part 1 --> Value of accumulator just before an instruction is executed twice
run bootCode 0 Set.empty 0 true |> printfn "%A"

// Part 2 --> Fix the program, what is the value of accumulator after executing fully
let changeOp (code: Instruction list) linenumber newOperation =
    let updated =
        List.find (fun inst -> inst.LineNumber = linenumber) code
        |> fun inst -> { inst with Operation = newOperation }

    let lastIndex = List.length code - 1

    match updated.LineNumber with
    | 0 -> updated :: code.[1..lastIndex]
    | l when l = lastIndex -> code.[0..lastIndex - 1] @ [ updated ]
    | _ ->
        code.[0..linenumber - 1]
        @ updated :: code.[linenumber + 1..lastIndex]

List.filter (fun inst ->
    inst.Operation = Operation.Jump
    || inst.Operation = Operation.NoOp) bootCode
|> Seq.map (fun inst ->
    match inst.Operation with
    | Operation.Jump -> changeOp bootCode inst.LineNumber Operation.NoOp
    | Operation.NoOp -> changeOp bootCode inst.LineNumber Operation.Jump
    | _ -> failwith "Unexpected operation in list")
|> Seq.tryPick (fun code -> run code 0 Set.empty 0 false)
|> printfn "%A"
