open System.IO
open System.Linq

type PasswordPolicy =
    { atLeast: int
      atMost: int
      letter: char }

let parse (line: string) =
    let words = line.Split " "
    let (atLeast, atMost) = words.[0] |> fun x -> x.Split "-" |> fun x -> int x.[0], int x.[1]
    let letter = words.[1].[0]
    let password = words.[2]

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
