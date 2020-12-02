open System.IO
open System.Linq

// The first or the second position must include the letter, but not both
type PasswordPolicy =
    { firstPos: int
      secondPos: int
      letter: char }

let parse (line: string) =
    let line = line.Split " "

    let (firstPos, secondPos) =
        line.[0]
        |> fun r ->
            r.Split "-"
            |> fun r -> int r.[0] - 1, int r.[1] - 1 // subtract one to normalize index (start at zero)

    let letter = line.[1].[0]
    let password = line.[2]

    ({ firstPos = firstPos
       secondPos = secondPos
       letter = letter },
     password)

let validate policy password =
    let first = Seq.tryItem policy.firstPos password
    let second = Seq.tryItem policy.secondPos password

    match (first, second) with
    | (Some first, Some second) when first = policy.letter && second = policy.letter -> None
    | (Some first, Some _) when first = policy.letter -> Some password
    | (Some _, Some second) when second = policy.letter -> Some password
    | (Some letter, _) -> if letter = policy.letter then Some password else None
    | (_, Some letter) -> if letter = policy.letter then Some password else None
    | _ -> None


File.ReadLines "puzzle.txt"
|> Seq.map parse
|> Seq.map (fun (policy, password) -> validate policy password)
|> Seq.choose id
|> Seq.length
|> printfn "%A"
