open System.IO

type Seat = int * int

let id ((row, col): Seat): int = row * 8 + col

let parse (line: string): Seat =
    let binarySearch letters (min, max) =
        letters
        |> Seq.fold (fun (min, max, _) el ->
            let delta = int (round (float (max - min) / 2.0))

            match el with
            | 'F'
            | 'L' -> (min, max - delta, Some el)
            | 'B'
            | 'R' -> (min + delta, max, Some el)
            | _ -> failwith "Unexpected char") (min, max, None)
        |> fun (min, max, last) ->
            match last with
            | Some 'F'
            | Some 'L' -> min
            | Some 'B'
            | Some 'R' -> max
            | _ -> failwith "Unexpected char"

    let row = binarySearch line.[0..6] (0, 127)
    let col = binarySearch line.[7..9] (0, 7)

    (row, col)

let boardingpasses =
    File.ReadLines "puzzle.txt" |> Seq.map parse

// part 1 -> Find the highest id
boardingpasses
|> Seq.map id
|> Seq.sortDescending
|> Seq.head
|> printfn "Highest seat id: %i"

// part 2 -> Find your seat
boardingpasses
|> Set.ofSeq
|> Set.difference  // all-seats minus those with boardingpasses
    ([ for row in 0 .. 127 do
        for col in 0 .. 7 do
            yield (row, col) ]
     |> Set.ofList)
|> Seq.find (fun seat -> // deal with missing seats
    let seatId = id seat

    Seq.exists (fun s -> id s = seatId + 1) boardingpasses
    && Seq.exists (fun s -> id s = seatId - 1) boardingpasses)
|> fun mySeat -> printfn "My seat: %A id %i" mySeat (id mySeat)
