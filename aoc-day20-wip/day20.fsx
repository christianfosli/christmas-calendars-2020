open System
open System.Text.RegularExpressions
open System.IO

type Pixel =
    | Hash
    | Dot

type TileImage = Pixel list list
type Tile = { Id: int; Image: TileImage }

module Tile =

    let ofString (str: string): Tile =
        let parseImage (lines: string array) =
            lines
            |> Array.map (fun row ->
                row
                |> Seq.map (fun c ->
                    match c with
                    | '#' -> Some Pixel.Hash
                    | '.' -> Some Pixel.Dot
                    | _ -> None)
                |> Seq.choose id
                |> List.ofSeq)
            |> List.ofArray

        let lines = str.Trim().Split("\n")

        let id =
            Regex.Match(lines.[0], "\\d+").Value |> int

        let image = Array.skip 1 lines |> parseImage

        { Id = id; Image = image }

    let flipHorizontal tile =
        { tile with
              Image = List.map (List.rev) tile.Image }

    let flipVertical tile =
        { tile with
              Image = List.rev tile.Image }

    let rotate180 = flipHorizontal >> flipVertical

    let rotate90 (tile: Tile) =
        let image = tile.Image
        let first = image.[0] |> List.map (fun x -> [ x ])

        let rotated =
            // This is kinda hard to read but makes sense on paper
            List.skip 1 image
            |> List.fold (fun acc el -> List.mapi (fun i x -> el.[i] :: x) acc) first

        { tile with Image = rotated }

    let mutations tile =
        // Some of these might be dupes, can't think
        let ninety = rotate90 tile
        let oneeighty = rotate180 tile
        let twoseventy = rotate90 oneeighty
        [ tile
          ninety
          oneeighty
          twoseventy
          flipHorizontal tile
          flipHorizontal ninety
          flipHorizontal oneeighty
          flipHorizontal twoseventy
          flipVertical tile
          flipVertical ninety
          flipVertical oneeighty
          flipVertical twoseventy ]

    let isVerticallyComposable topTile bottomTile =
        mutations topTile
        |> List.exists (fun top ->
            let topsBottom = List.last top.Image
            List.exists (fun bottom -> (List.head bottom.Image) = topsBottom) (mutations bottomTile))

    let isHorizontallyComposable leftTile rightTile =
        mutations leftTile
        |> List.exists (fun left ->
            let leftsRight =
                rotate90 left
                |> fun lr -> List.last lr.Image

            List.exists (fun right ->
                (rotate90 right
                 |> fun r -> List.head r.Image) = leftsRight) (mutations rightTile))


let tiles =
    File.ReadAllText "input.txt"
    |> fun text -> text.Trim().Split("\n\n")
    |> Array.map Tile.ofString

// Part 1
// TODO: So I guess I need to try every combination of tiles together with
// every combination of tiles per row...
// Uuuh, that's work

// Tests
tiles.[0]
|> Tile.rotate180
|> Tile.rotate180
|> fun t ->
    if t.Image = tiles.[0].Image
    then ()
    else failwithf "Expected %A but got %A" tiles.[0].Image t.Image

tiles.[0]
|> Tile.rotate90
|> Tile.rotate90
|> fun t ->
    if t.Image =
        (Tile.rotate180 tiles.[0]
         |> fun x -> x.Image) then
        ()
    else
        failwithf "%A was rotated 90deg twice but does not equal 180 deg rotate" t
