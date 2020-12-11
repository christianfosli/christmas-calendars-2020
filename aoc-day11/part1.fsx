open System.IO

module Position =
    type T =
        | Floor
        | EmptySeat
        | OccupiedSeat

    let ofChar ch =
        match ch with
        | '.' -> Floor
        | 'L' -> EmptySeat
        | '#' -> OccupiedSeat
        | _ -> failwith "Invalid character"

module Seq =
    let ofArray2D a =
        seq {
            for i in 0 .. Array2D.length1 a - 1 do
                for j in 0 .. Array2D.length2 a - 1 do
                    yield a.[i, j]
        }

let parse (seatLayout: string list) =
    let parse (line: string) =
        Seq.map Position.ofChar line |> List.ofSeq

    List.map parse seatLayout |> array2D

let adjacentSeats i j a =
    let tryGet i j (a: Position.T [,]) =
        try
            Some a.[i, j]
        with _ -> None

    [ tryGet (i - 1) j a
      tryGet (i + 1) j a
      tryGet i (j - 1) a
      tryGet i (j + 1) a
      tryGet (i - 1) (j - 1) a
      tryGet (i + 1) (j + 1) a
      tryGet (i + 1) (j - 1) a
      tryGet (i - 1) (j + 1) a ]
    |> List.choose id

let nextLayout (current: Position.T [,]) =
    let mutable next = Array2D.copy current

    for i in 0 .. Array2D.length1 current - 1 do
        for j in 0 .. Array2D.length2 current - 1 do
            match current.[i, j] with
            | Position.EmptySeat when not (Seq.exists (fun x -> x = Position.OccupiedSeat) (adjacentSeats i j current)) ->
                next.[i, j] <- Position.OccupiedSeat
            | Position.OccupiedSeat when (Seq.filter (fun x -> x = Position.OccupiedSeat) (adjacentSeats i j current)
                                          |> fun x -> Seq.length x >= 4) -> next.[i, j] <- Position.EmptySeat
            | _ -> ()

    next

let rec predictStableLayout (current: Position.T [,]) =
    let next = nextLayout current
    let nextnext = nextLayout next
    if next = nextnext then next else predictStableLayout next

File.ReadLines "input.txt"
|> List.ofSeq
|> parse
|> predictStableLayout
|> Seq.ofArray2D
|> Seq.filter (fun x -> x = Position.OccupiedSeat)
|> Seq.length
|> printfn "%A"
