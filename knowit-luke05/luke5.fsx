#r "nuget: Plotly.NET,2.0.0-alpha2"

open System.IO
open System
open Plotly.NET

module Direction =

    type T =
        | Right
        | Left
        | Up
        | Down

    let ofChar char =
        match char with
        | 'H' -> Right
        | 'V' -> Left
        | 'O' -> Up
        | 'N' -> Down
        | unexpected -> failwithf "Character %c is not a direction" unexpected

type Coordinate = int * int

let coordinates =
    File.ReadAllText "rute.txt"
    |> Seq.map Direction.ofChar
    |> Seq.fold (fun (acc, prevDir) dir ->
        let (prevX, prevY) = List.head acc

        let c =
            match dir with
            | Direction.Right -> (prevX + 1, prevY)
            | Direction.Left -> (prevX - 1, prevY)
            | Direction.Up -> (prevX, prevY + 1)
            | Direction.Down -> (prevX, prevY - 1)

        match prevDir with
        | Some d when d = dir -> (List.skip 1 acc |> fun acc -> (c :: acc), Some dir)
        | _ -> (c :: acc), Some dir) ([ (0, 0) ], None)
    |> fun (cords, _) -> cords
    |> List.rev

// Plot with plotly
printfn "Plotting santas house... --> Check plot.html"

coordinates
|> Chart.Line
|> Chart.SaveHtmlAs "plot"

// The plot was too complex to visually calculate the area... Stealing impl of shoelace algorithm I found on google:
// https://rosettacode.org/wiki/Shoelace_formula_for_polygonal_area#F.23
let shoelace (n :: g) =
    abs
        (List.pairwise (n :: g @ [ n ])
         |> List.fold (fun n ((nα, gα), (nβ, gβ)) -> n + (nα * gβ) - (gα * nβ)) 0)
    / 2

coordinates
|> shoelace
|> printfn "Area of santas house: %i"
