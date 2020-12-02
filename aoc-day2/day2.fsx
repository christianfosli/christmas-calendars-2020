open System.IO
open System.Linq

type PasswordPolicy =
    { atLeast: int
      atMost: int
      letter: char }

let parse (line: string) =
    let line = line.Split " "
    let (atLeast, atMost) = line.[0] |> fun r -> r.Split "-" |> fun r -> int r.[0], int r.[1]
    let letter = line.[1].[0]
    let password = line.[2]

    ({ atLeast = atLeast
       atMost = atMost
       letter = letter },
     password)

let validate policy password =
    password
    |> Seq.filter (fun char -> char = policy.letter)
    |> Seq.length
    |> fun length ->
        if length >= policy.atLeast
           && length <= policy.atMost then
            Some password
        else
            None

File.ReadLines "puzzle.txt"
|> Seq.map parse
|> Seq.map (fun (policy, password) -> validate policy password)
|> Seq.choose id
|> Seq.length
|> printfn "%A"
