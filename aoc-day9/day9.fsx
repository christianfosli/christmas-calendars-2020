open System.IO

let rec pairCombinations list =
    match list with
    | [] -> []
    | head :: tail ->
        List.map (fun el -> (head, el)) tail
        @ pairCombinations tail

let rec validateXmas preamble numbers =
    match Seq.tryHead numbers with
    | None -> Ok()
    | Some num when preamble
                    |> pairCombinations
                    |> List.exists (fun (x, y) -> x + y = num) ->
        validateXmas (num :: preamble |> List.take 25) (Seq.skip 1 numbers)
    | Some invalidNum -> Error invalidNum


let (preamble, rest) =
    File.ReadLines "input.txt"
    |> Seq.map int
    |> fun x -> (Seq.take 25 x |> List.ofSeq, Seq.skip 25 x)

// Part 1 -> What is the first invalid number
validateXmas preamble rest |> printfn "%A"
