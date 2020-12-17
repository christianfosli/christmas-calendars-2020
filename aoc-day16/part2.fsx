open System
open System.IO

// ****
// --- WIP Warning --- Not Yet Finished ---
// ****

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

let validateTicket (rules: FieldRule.T seq) (ticket: string) =
    let validateField (rules: FieldRule.T seq) (field: int) =
        if Seq.exists (fun r -> FieldRule.validate field r |> Option.isSome) rules
        then Some field
        else None

    let fields =
        ticket.Split(",") |> Array.map int |> List.ofArray

    if List.forall (fun f -> validateField rules f |> Option.isSome) fields
    then Some fields
    else None

let crackFields (rules: FieldRule.T seq) (validTickets: (int list) seq) =
    let numberOfFields = Seq.length rules

    let potentialFields (rule: FieldRule.T) (tickets: (int list) seq) =
        [ 0 .. numberOfFields - 1 ]
        |> List.filter (fun i ->
            Seq.map (fun (t: int list) -> t.[i]) tickets
            |> Seq.forall (fun f -> FieldRule.validate f rule |> Option.isSome))

    let possibleFieldsPerRule =
        Seq.map (fun r -> (r, potentialFields r validTickets)) rules

    // TODO: I got stuck narrowing down the correct fields
    // I now have a list of valid fieldnumbers for every rule, by itself,
    // but I need to trim those down to one fieldnumber per rule
    // which sounds like a reasonable task but... ¯\_(ツ)_/¯
    possibleFieldsPerRule |> List.ofSeq

let rules, myTicket, nearbyTickets =
    File.ReadAllText "input.txt"
    |> fun text ->
        text.Split("\n\n")
        |> fun text ->
            text.[0].Trim().Split("\n")
            |> Seq.map FieldRule.parse,
            text.[1].Trim().Split("\n").[1],
            text.[2].Trim().Split("\n") |> Seq.skip 1

let validTickets =
    nearbyTickets
    |> Seq.map (fun ticket -> validateTicket rules ticket)
    |> Seq.choose id

crackFields rules validTickets
|> List.filter (fun (rule, fieldnumbers) -> rule.Name.StartsWith("departure"))
|> printfn "%A"
