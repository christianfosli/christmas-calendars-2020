open System.IO

type Ingredients =
    { Sugar: int
      Flour: int
      Milk: int
      Egg: int }

let rec bakeCakes (currentCakes: int) (ingredients: Ingredients) =
    if ingredients.Sugar >= 2
       && ingredients.Flour >= 3
       && ingredients.Milk >= 3
       && ingredients.Egg >= 1 then
        bakeCakes
            (currentCakes + 1)
            { Sugar = ingredients.Sugar - 2
              Flour = ingredients.Flour - 3
              Milk = ingredients.Milk - 3
              Egg = ingredients.Egg - 1 }
    else
        (currentCakes, ingredients)

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
|> fun ingredients -> bakeCakes 0 ingredients
|> printfn "%A"
