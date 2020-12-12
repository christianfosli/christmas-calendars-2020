open System.IO

// the built in '%' operator in dotnet doesn't handle negative numbers properly
let inline (%!) a b = (a % b + b) % b

module Direction =
    type T =
        | North
        | East
        | South
        | West

    let toDeg dir =
        match dir with
        | North -> 0
        | East -> 90
        | South -> 180
        | West -> 270

    let ofDeg d =
        match d %! 360 with
        | 0 -> North
        | 90 -> East
        | 180 -> South
        | 270 -> West
        | _ -> failwithf "don't know which direction %i is" d

    let turnRight degrees dir = toDeg dir + degrees |> ofDeg

    let turnLeft degrees dir = toDeg dir - degrees |> ofDeg

File.ReadLines "input.txt"
|> Seq.map (fun (ins: string) -> (ins.Substring(0, 1), ins.Substring(1) |> int))
|> Seq.fold (fun (acc, cur) (ins, n) ->
    // Couldn't get intelisense working inside this fn, but it runs fine
    // something about Map.change being added recently. It could probably be written nicer...
    let add v =
        match v with
        | Some v -> Some(v + n)
        | None -> None

    match ins with
    | "N" -> Map.change Direction.North add acc, cur
    | "E" -> Map.change Direction.East add acc, cur
    | "S" -> Map.change Direction.South add acc, cur
    | "W" -> Map.change Direction.West add acc, cur
    | "L" -> acc, Direction.turnLeft n cur
    | "R" -> acc, Direction.turnRight n cur
    | "F" -> Map.change cur add acc, cur
    | _ -> failwith "unexpected instruction")
       ([ (Direction.North, 0)
          (Direction.East, 0)
          (Direction.South, 0)
          (Direction.West, 0) ]
        |> Map.ofSeq,
        Direction.East)
|> fun (x, _) ->
    abs (x.[Direction.North] - x.[Direction.South])
    + abs (x.[Direction.East] - x.[Direction.West])
|> printfn "%i"
