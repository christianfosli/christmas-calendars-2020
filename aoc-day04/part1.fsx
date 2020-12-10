open System.IO

type Passport =
    { BirthYear: string
      IssueYear: string
      ExpirationYear: string
      Height: string
      HairColor: string
      EyeColor: string
      PassportId: string
      CountryId: string option }

let parse (passportdata: string): Passport option =
    passportdata.Replace("\n", " ")
    |> fun data -> data.Trim()
    |> fun data -> data.Split " "
    |> Seq.map (fun field -> field.Split ":" |> fun kv -> (kv.[0], kv.[1]))
    |> Map.ofSeq
    |> fun fields ->
        match (Map.tryFind "byr" fields,
               Map.tryFind "iyr" fields,
               Map.tryFind "eyr" fields,
               Map.tryFind "hgt" fields,
               Map.tryFind "hcl" fields,
               Map.tryFind "ecl" fields,
               Map.tryFind "pid" fields) with
        | (Some byr, Some iyr, Some eyr, Some hgt, Some hcl, Some ecl, Some pid) ->
            Some
                { BirthYear = byr
                  IssueYear = iyr
                  ExpirationYear = eyr
                  Height = hgt
                  HairColor = hcl
                  EyeColor = ecl
                  PassportId = pid
                  CountryId = Map.tryFind "cid" fields }
        | _ -> None

File.ReadAllText "puzzle.txt"
|> fun text -> text.Split "\n\n"
|> Seq.map parse
|> Seq.choose id
|> Seq.length
|> printfn "%i"
