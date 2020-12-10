open System.IO

type Accumulator = { Last: int; Ones: int; Threes: int }

// Part 1
File.ReadLines "input.txt"
|> Seq.map int
|> List.ofSeq
|> List.sort
|> fun l -> l @ [ List.max l + 3 ]
|> List.fold (fun acc el ->
    match el - int acc.Last with
    | 3 ->
        { Last = el
          Ones = acc.Ones
          Threes = acc.Threes + 1 }
    | 2 -> { acc with Last = el }
    | 1 ->
        { Last = el
          Ones = acc.Ones + 1
          Threes = acc.Threes }
    | _ -> failwith "Incompatible adapters!") { Last = 0; Ones = 0; Threes = 0 }
|> fun x -> x.Ones * x.Threes
|> printfn "%A"
// -
// Skipping part 2 today

