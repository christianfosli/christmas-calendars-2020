open System.IO
open System.Linq

// The first or the second position must include the letter, but not both
type PasswordPolicy =
    { firstPos: int
      secondPos: int
      letter: char }

let parse (line: string) =
    let words = line.Split " "

    let (firstPos, secondPos) =
        words.[0]
        |> fun x ->
            x.Split "-"
            |> fun x -> int x.[0] - 1, int x.[1] - 1 // subtract one to normalize index (start at zero)

    let letter = words.[1].[0]
    let password = words.[2]

    ({ firstPos = firstPos
       secondPos = secondPos
       letter = letter },
     password)

let validate policy password =
    let first = Seq.tryItem policy.firstPos password
    let second = Seq.tryItem policy.secondPos password

    match (first, second) with
    | (Some first, Some second) when first = policy.letter && second = policy.letter -> None
    | (Some first, _) when first = policy.letter -> Some password
    | (_, Some second) when second = policy.letter -> Some password
    | _ -> None


File.ReadLines "puzzle.txt"
|> Seq.map parse
|> Seq.map (fun (policy, password) -> validate policy password)
|> Seq.choose id
|> Seq.length
|> printfn "%A"
