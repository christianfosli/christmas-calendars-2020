open System.IO
open System.Linq

type PasswordPolicy =
    { atLeast: int
      atMost: int
      letter: char }

let parse (line: string) =
    let line = line.Split " "
    let range = line.[0] |> fun r -> r.Split "-"
    let atLeast = int range.[0]
    let atMost = int range.[1]
    let letter = line.[1].[0]
    let password = line.[2]

    ({ atLeast = atLeast
       atMost = atMost
       letter = letter },
     password)

let validate (policy: PasswordPolicy) (password: string) =
    match password.Count(fun c -> c = policy.letter) with
    | count when count >= policy.atLeast && count <= policy.atMost -> Some password
    | _ -> None

File.ReadLines "puzzle.txt"
|> Seq.map parse
|> Seq.map (fun (policy, password) -> validate policy password)
|> Seq.choose id
|> Seq.length
|> printfn "%A"
