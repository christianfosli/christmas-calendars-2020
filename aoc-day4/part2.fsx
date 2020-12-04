open System.IO
open System.Text.RegularExpressions

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

let validate passport =
    try
        if int passport.BirthYear >= 1920
           && int passport.BirthYear <= 2002
           && int passport.IssueYear >= 2010
           && int passport.IssueYear <= 2020
           && int passport.ExpirationYear >= 2020
           && int passport.ExpirationYear <= 2030
           && match passport.Height.[String.length passport.Height - 2..String.length passport.Height - 1] with
              | "cm" ->
                  let height =
                      int passport.Height.[0..passport.Height.Length - 3]

                  if height >= 150 && height <= 193 then true else false
              | "in" ->
                  let height =
                      int passport.Height.[0..passport.Height.Length - 3]

                  if height >= 59 && height <= 76 then true else false
              | _ -> false
           && Regex.IsMatch(passport.HairColor, @"^#[0-9a-f]{6}$")
           && match passport.EyeColor with
              | "amb"
              | "blu"
              | "brn"
              | "gry"
              | "grn"
              | "hzl"
              | "oth" -> true
              | _ -> false
           && Regex.IsMatch(passport.PassportId, @"^[0-9]{9}$") then
            Some passport
        else
            None
    with ex ->
        printfn "Error during validation: %A" ex
        None

File.ReadAllText "puzzle.txt"
|> fun text -> text.Split "\n\n"
|> Seq.map parse
|> Seq.choose id
|> Seq.map validate
|> Seq.choose id
|> Seq.length
|> printfn "%i"
