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

type Direction =
    | North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest

let parse (seatLayout: string list) =
    let parse (line: string) =
        Seq.map Position.ofChar line |> List.ofSeq

    List.map parse seatLayout |> array2D

let visibleSeats i j a =
    let tryGet i j (a: Position.T [,]) =
        try
            Some a.[i, j]
        with _ -> None

    let rec tryGetFirstVisible i j dir a =
        // TODO: DRY
        match dir with
        | North ->
            match tryGet (i - 1) j a with
            | Some v when v = Position.Floor -> tryGetFirstVisible (i - 1) j North a
            | Some v -> Some v
            | None -> None
        | NorthEast ->
            match tryGet (i - 1) (j + 1) a with
            | Some v when v = Position.Floor -> tryGetFirstVisible (i - 1) (j + 1) NorthEast a
            | Some v -> Some v
            | None -> None
        | East ->
            match tryGet i (j + 1) a with
            | Some v when v = Position.Floor -> tryGetFirstVisible i (j + 1) East a
            | Some v -> Some v
            | None -> None
        | SouthEast ->
            match tryGet (i + 1) (j + 1) a with
            | Some v when v = Position.Floor -> tryGetFirstVisible (i + 1) (j + 1) SouthEast a
            | Some v -> Some v
            | None -> None
        | South ->
            match tryGet (i + 1) j a with
            | Some v when v = Position.Floor -> tryGetFirstVisible (i + 1) j South a
            | Some v -> Some v
            | None -> None
        | SouthWest ->
            match tryGet (i + 1) (j - 1) a with
            | Some v when v = Position.Floor -> tryGetFirstVisible (i + 1) (j - 1) SouthWest a
            | Some v -> Some v
            | None -> None
        | West ->
            match tryGet i (j - 1) a with
            | Some v when v = Position.Floor -> tryGetFirstVisible i (j - 1) West a
            | Some v -> Some v
            | None -> None
        | NorthWest ->
            match tryGet (i - 1) (j - 1) a with
            | Some v when v = Position.Floor -> tryGetFirstVisible (i - 1) (j - 1) NorthWest a
            | Some v -> Some v
            | None -> None

    [ tryGetFirstVisible i j North a
      tryGetFirstVisible i j NorthEast a
      tryGetFirstVisible i j East a
      tryGetFirstVisible i j SouthEast a
      tryGetFirstVisible i j South a
      tryGetFirstVisible i j SouthWest a
      tryGetFirstVisible i j West a
      tryGetFirstVisible i j NorthWest a ]
    |> List.choose id

let nextLayout current =
    let mutable next = Array2D.copy current

    for i in 0 .. Array2D.length1 current - 1 do
        for j in 0 .. Array2D.length2 current - 1 do
            match current.[i, j] with
            | Position.EmptySeat when not (Seq.exists (fun x -> x = Position.OccupiedSeat) (visibleSeats i j current)) ->
                next.[i, j] <- Position.OccupiedSeat
            | Position.OccupiedSeat when (Seq.filter (fun x -> x = Position.OccupiedSeat) (visibleSeats i j current)
                                          |> fun x -> Seq.length x >= 5) -> next.[i, j] <- Position.EmptySeat
            | _ -> ()

    next

let rec predictStableLayout current =
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
