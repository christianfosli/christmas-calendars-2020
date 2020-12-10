open System.IO

let rec pairCombinations lst =
    match lst with
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

let findFirstContigiousSetWithSum sum numbers =
    numbers
    |> Seq.fold (fun acc el ->
        if List.sum acc = sum then
            acc
        elif List.sum (el :: acc) <= sum then
            el :: acc
        elif el >= sum then
            []
        else
            let rec whileTooBigDropTail n lst =
                let sum = List.sum lst

                if sum <= n then
                    lst
                else
                    (List.length lst - 1, lst)
                    |> fun (count, lst) -> List.take count lst
                    |> whileTooBigDropTail n

            whileTooBigDropTail sum (el :: acc)) []

let input =
    File.ReadLines "input.txt" |> Seq.map int64

// Part 1 -> What is the first invalid number
let preamble, rest =
    (Seq.take 25 input |> List.ofSeq, Seq.skip 25 input)

let firstInvalid =
    match validateXmas preamble rest with
    | Ok () -> failwith "There was supposed to be an invalid number here"
    | Error n ->
        printfn "%A" n
        n

// Part 2 -> find a contigious set of 2+ numbers that sum up to our previous answer,
//        -> sum the smallest and largest of these numbers
input
|> findFirstContigiousSetWithSum firstInvalid
|> Seq.sort
|> fun x -> Seq.head x + Seq.head (Seq.rev x)
|> printfn "%A"
