open System.IO

// Multiply the number of trees encountered on each slope

type T = { Position: int; Trees: int }

type Slope = int * int

let treehit map slope =
    let right, down = slope

    map
    |> Seq.mapi (fun i el -> el, i)
    |> Seq.filter (fun (_, i) -> i % down = 0)
    |> Seq.map fst
    |> Seq.fold (fun acc el ->
        match Seq.item (acc.Position % Seq.length el) el with
        | '#' ->
            { Position = acc.Position + right
              Trees = acc.Trees + 1 }
        | _ ->
            { Position = acc.Position + right
              Trees = acc.Trees }) { Position = 0; Trees = 0 }
    |> fun res -> res.Trees

let map = File.ReadLines "puzzle.txt"

[ (1, 1)
  (3, 1)
  (5, 1)
  (7, 1)
  (1, 2) ]
|> Seq.map (fun slope -> treehit map slope)
|> Seq.reduce (*)
|> printfn "%i"
