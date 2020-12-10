open System
open System.IO

// Given a list of ingredients, how many cakes where made this year?

type Ingredients =
    { Sugar: int
      Flour: int
      Milk: int
      Egg: int }

    static member (+)(i1, i2) =
        { Sugar = i1.Sugar + i2.Sugar
          Flour = i1.Flour + i2.Flour
          Milk = i1.Milk + i2.Milk
          Egg = i1.Egg + i2.Egg }

// returns (numberOfCakes, leftOverIngredients)
let bakeCakes ingredients =
    let numberOfCakes =
        [ ingredients.Sugar / 2
          ingredients.Flour / 3
          ingredients.Milk / 3
          ingredients.Egg ]
        |> Seq.sort
        |> Seq.head

    (numberOfCakes,
     { Sugar = ingredients.Sugar - numberOfCakes * 2
       Flour = ingredients.Flour - numberOfCakes * 3
       Milk = ingredients.Milk - numberOfCakes * 3
       Egg = ingredients.Egg - numberOfCakes })

let parse (line: string) =
    line.Replace(" ", "")
    |> fun line -> line.Split ","
    |> Seq.map (fun kv -> kv.Split ":" |> fun kv -> (kv.[0], int kv.[1]))
    |> Map.ofSeq
    |> fun ingredients ->
        { Sugar =
              (Map.tryFind "sukker" ingredients
               |> Option.defaultValue 0)
          Flour =
              (Map.tryFind "mel" ingredients
               |> Option.defaultValue 0)
          Milk =
              (Map.tryFind "melk" ingredients
               |> Option.defaultValue 0)
          Egg =
              Map.tryFind "egg" ingredients
              |> Option.defaultValue 0 }

File.ReadLines "leveringsliste.txt"
|> Seq.map parse
|> Seq.reduce (+)
|> bakeCakes
|> printfn "%A"
