open System.IO

let parse (busses: string) =
    busses.Split(",")
    |> Array.map (fun bus ->
        match bus with
        | "x" -> None
        | b -> int b |> Some)

let nextDep (timestamp: int) (buss: int) =
    buss
    * int (ceil ((float timestamp) / (float buss)))

let earliestDep, busses =
    File.ReadAllLines "input.txt"
    |> fun x -> int x.[0], (x.[1] |> parse |> List.ofArray)

// part 1
busses
|> List.choose id
|> List.sortBy (fun b -> nextDep earliestDep b)
|> List.head
|> fun firstBus ->
    let wait =
        nextDep earliestDep firstBus - earliestDep

    printfn "Buss %i with %i min wait. Multiplied: %i" firstBus wait (firstBus * wait)

// part 2
let testSeq busses timestamp =
    busses
    |> List.map (fun b -> Option.map (nextDep timestamp) b)
    |> List.mapi (fun i el -> (el, i))
    |> List.forall (fun (el, i) ->
        match el with
        | Some t -> t - timestamp = i
        | None -> true)

let firstBus =
    List.head busses |> Option.defaultValue 0

let firstBusDepartures =
    Seq.initInfinite (fun t -> t + firstBus)

// This takes too long to calculate so we don't get anywhere...
//firstBusDepartures
//|> Seq.find (fun ts -> testSeq busses ts)
//|> printfn "%A"
//
// It works on the testcase:
let testBusses =
    [ Some 7
      Some 13
      None
      None
      Some 59
      None
      Some 31
      Some 19 ]

let firstTestBus =
    List.head testBusses |> Option.defaultValue 0

let firstTestBusDeps =
    Seq.initInfinite (fun t -> nextDep (t + 1) firstTestBus)

firstTestBusDeps
|> Seq.find (fun ts -> testSeq testBusses ts)
|> printfn "%A"
