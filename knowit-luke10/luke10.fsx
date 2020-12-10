open System.IO

let points placement attendeeCount = attendeeCount - placement

let parse (exerciseLine: string) =
    let players = exerciseLine.Split(",")

    players
    |> List.ofArray
    |> List.mapi (fun i player ->
        points (i + 1) (Array.length players)
        |> fun points -> (player, points))

let addPoints ((player, points): string * int) (m: Map<string, int>) =
    let points =
        match Map.tryFind player m with
        | Some v -> v + points
        | None -> points

    Map.add player points m

let rec addManyPoints l m =
    match l with
    | [] -> m
    | head :: tail -> addPoints head m |> addManyPoints tail


File.ReadLines "leker.txt"
|> Seq.map parse
|> List.ofSeq
|> List.fold (fun acc el -> addManyPoints el acc) Map.empty
|> Seq.maxBy (fun x -> x.Value)
|> printfn "%A"
