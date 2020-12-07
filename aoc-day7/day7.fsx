open System.IO
open System.Text.RegularExpressions

type Color = Color of string

type BagRule =
    { Color: Color
      MustContain: (int * Color) list }

let parse (line: string) =
    Regex.Split(line, "contain")
    |> fun line ->
        (line.[0],
         match line.[1].Contains(',') with
         | true -> line.[1].Split(',') |> List.ofArray
         | false when Regex.IsMatch(line.[1], "no other bags") -> []
         | false -> [ line.[1] ])
    |> fun (bag, contains) ->
        let bagColor = bag.Replace("bags", "").Trim() |> Color

        let canContain =
            contains
            |> List.map (fun c ->
                let c = c.Trim()
                let number = int c.[0] - int '0' // subtract '0' to normalize ascii
                let color = Color c.[2..(c.IndexOf("bag") - 2)]
                (number, color))

        { Color = bagColor
          MustContain = canContain }

let rules =
    File.ReadLines "puzzle.txt"
    |> Seq.map parse
    |> List.ofSeq

// part 1 -> How many bag colors eventually contain at least one shiny gold bag?
let rec canContainColor (color: Color) (bag: BagRule) =
    if List.exists (fun (_, col) -> col = color) bag.MustContain then
        true
    else
        let children =
            List.map (fun (_, col) -> List.find (fun br -> br.Color = col) rules) bag.MustContain

        List.exists (fun child -> canContainColor color child) children

rules
|> List.filter (fun br -> canContainColor (Color "shiny gold") br)
|> List.length
|> printfn "Number of bags that can contain shiny gold: %i"

// part 2 -> How many bags are inside my shiny gold bag?
let rec countBagsWithinBags (bag: BagRule) =
    let children =
        bag.MustContain
        |> List.map (fun (n, col) ->
            List.find (fun b -> b.Color = col) rules
            |> fun childBag -> (n, childBag))

    children
    |> List.sumBy (fun (count, child) -> count + count * (countBagsWithinBags child))


rules
|> List.find (fun br -> br.Color = Color "shiny gold")
|> countBagsWithinBags
|> printfn "Number of bags required inside my shiny gold bag: %i"
