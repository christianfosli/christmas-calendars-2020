#r "nuget: FSharp.Data"
open FSharp.Data

// The file contains numbers 1-100_000 in a random order, except for one number.
// Find the missing number

Http.RequestString
    "https://julekalender-backend.knowit.no/challenges/1/attachments/numbers.txt"
|> fun numberString -> numberString.Split ","
|> Seq.map int
|> Set.ofSeq
|> Set.difference (Set.ofSeq [ 1 .. 100_000 ])
|> printfn "%A"
