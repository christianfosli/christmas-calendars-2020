open System.IO

// How many trees are hit when traversing the map below,
// starting at top left and traversing with slope (right 3, down 1)

type T = { Position: int; Trees: int }

File.ReadLines "puzzle.txt"
|> Seq.fold (fun acc el ->
    match Seq.item (acc.Position % Seq.length el) el with
    | '#' ->
        { Position = acc.Position + 3
          Trees = acc.Trees + 1 }
    | _ ->
        { Position = acc.Position + 3
          Trees = acc.Trees }) { Position = 0; Trees = 0 }
|> printfn "%A"
