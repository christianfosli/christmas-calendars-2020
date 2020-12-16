open System
open System.IO

module FieldRule =
    type Range = int * int // min, max  Inclusive!

    type T =
        { Name: string
          Range1: Range
          Range2: Range }

    let validate (field: int) (rule: T) =
        let (min1, max1), (min2, max2) = rule.Range1, rule.Range2

        if (field >= min1 && field <= max1)
           || (field >= min2 && field <= max2) then
            Some field
        else
            None

    let parse (line: string): T =
        let parseRange (range: string): Range =
            range.Trim().Split("-")
            |> fun range -> int range.[0], int range.[1]

        let parseRanges (ranges: string): (Range * Range) =
            let (range1, range2) =
                ranges.Split("or") |> fun r -> r.[0], r.[1]

            (parseRange range1, parseRange range2)

        line.Split(":")
        |> fun line -> line.[0], parseRanges line.[1]
        |> fun (name, (r1, r2)) ->
            { Name = name
              Range1 = r1
              Range2 = r2 }

let validateTicketFields (rules: FieldRule.T seq) (ticket: string) =
    let validateField (rules: FieldRule.T seq) (field: int) =
        if Seq.exists (fun r -> FieldRule.validate field r |> Option.isSome) rules
        then Ok field
        else Error field

    let fields = ticket.Split(",") |> Seq.map int

    Seq.map (fun f -> validateField rules f) fields

let rules, myTicket, nearbyTickets =
    File.ReadAllText "input.txt"
    |> fun text ->
        text.Split("\n\n")
        |> fun text ->
            text.[0].Trim().Split("\n")
            |> Seq.map FieldRule.parse,
            text.[1].Trim().Split("\n").[1],
            text.[2].Trim().Split("\n") |> Seq.skip 1

nearbyTickets
|> Seq.map (fun ticket -> validateTicketFields rules ticket)
|> Seq.fold (fun acc el ->
    Seq.map (fun r ->
        match r with
        | Ok _ -> None
        | Error field -> Some field) el
    |> Seq.choose id
    |> Seq.sum
    |> fun sum -> acc + sum) 0
|> printfn "Error rate (sum of invalid fields): %i"
