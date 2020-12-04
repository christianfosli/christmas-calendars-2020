open System
open System.IO

// Given a list of ingredients, how many cakes where made this year?

type Ingredients =
    { Sugar: int
      Flour: int
      Milk: int
      Egg: int }

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

File.ReadLines "leveringsliste.txt"
|> Seq.map (fun line ->
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
              |> Option.defaultValue 0 })
|> Seq.reduce (fun acc el ->
    { Sugar = acc.Sugar + el.Sugar
      Flour = acc.Flour + el.Flour
      Milk = acc.Milk + el.Milk
      Egg = acc.Egg + el.Egg })
|> bakeCakes
|> printfn "%A"
