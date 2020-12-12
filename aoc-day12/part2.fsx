open System.IO

// -- I couldn't get intelisense working with dotnet 5, so it's a little ugly --

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
|> Seq.fold (fun (ship, waypoint) (ins, n) ->
    // Couldn't get intelisense working inside this fn, but it runs fine
    // something about Map.change being added recently. It could probably be written nicer...
    let add v =
        match v with
        | Some v -> Some(v + n)
        | None -> None

    let rec moveForward (ship: Map<Direction.T, int>) (n: int) (waypoint: Map<Direction.T, int>) =
        if n = 0 then
            ship
        else
            Map.add
                Direction.North
                (ship.[Direction.North]
                 + waypoint.[Direction.North])
                ship
            |> Map.add Direction.East (ship.[Direction.East] + waypoint.[Direction.East]) 
            |> Map.add
                Direction.South
                   (ship.[Direction.South]
                    + waypoint.[Direction.South])
            |> Map.add Direction.West (ship.[Direction.West] + waypoint.[Direction.West])
            |> fun sh -> moveForward sh (n - 1) waypoint
    
    let rotateWaypointRight (degrees: int) (waypoint: Map<Direction.T, int>) =
        [ Direction.turnRight degrees Direction.North, waypoint.[Direction.North]
          Direction.turnRight degrees Direction.East, waypoint.[Direction.East]
          Direction.turnRight degrees Direction.South, waypoint.[Direction.South]
          Direction.turnRight degrees Direction.West, waypoint.[Direction.West] ]
        |> Map.ofSeq

    let rotateWaypointLeft (degrees: int) (waypoint: Map<Direction.T, int>) =
        [ Direction.turnLeft degrees Direction.North, waypoint.[Direction.North]
          Direction.turnLeft degrees Direction.East, waypoint.[Direction.East]
          Direction.turnLeft degrees Direction.South, waypoint.[Direction.South]
          Direction.turnLeft degrees Direction.West, waypoint.[Direction.West] ]
        |> Map.ofSeq

    match ins with
    | "N" -> ship, Map.change Direction.North add waypoint
    | "E" -> ship, Map.change Direction.East add waypoint
    | "S" -> ship, Map.change Direction.South add waypoint
    | "W" -> ship, Map.change Direction.West add waypoint
    | "L" -> ship, rotateWaypointLeft n waypoint
    | "R" -> ship, rotateWaypointRight n waypoint
    | "F" -> moveForward ship n waypoint, waypoint
    | _ -> failwith "unexpected instruction")
       ([ (Direction.North, 0)
          (Direction.East, 0)
          (Direction.South, 0)
          (Direction.West, 0) ]
        |> Map.ofSeq,
        [ (Direction.North, 1)
          (Direction.East, 10)
          (Direction.South, 0)
          (Direction.West, 0) ]
        |> Map.ofSeq)
|> fun (x, _) ->
    abs (x.[Direction.North] - x.[Direction.South])
    + abs (x.[Direction.East] - x.[Direction.West])
|> printfn "%i"
