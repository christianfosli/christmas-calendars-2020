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
    // TODO: This needs some cleanup!!!
    let numberOfFields = Seq.length rules

    let potentialFields (rule: FieldRule.T) (tickets: (int list) seq) =
        [ 0 .. numberOfFields - 1 ]
        |> List.filter (fun i ->
            Seq.map (fun (t: int list) -> t.[i]) tickets
            |> Seq.forall (fun f -> FieldRule.validate f rule |> Option.isSome))

    let possibilities =
        Seq.map (fun r -> (r, potentialFields r validTickets)) rules

    possibilities |> List.ofSeq
//|> List.map (fun num -> (num, Seq.filter (fun (r, l) -> List.exists (fun n -> n = num)) l) possibilities)

//    let rec tryToFindExactFields (map: Map<FieldRule.T, int list>) (evicted: int list) =
//        if Map.forall (fun _ fieldNumbers -> (List.length fieldNumbers) = 1) map then
//            map
//        else
//            let sortedExceptSinglesAndAllEvicted =
//                Map.toList map
//                |> List.sortBy (fun (_, fieldNumbers) -> List.length fieldNumbers)
//                |> List.skipWhile (fun (_, fieldNumbers) -> (List.length fieldNumbers) = 1)
//                |> List.filter (fun (_, fieldNumbers) ->
//                    not (List.forall (fun field -> List.exists (fun ev -> ev = field) evicted) fieldNumbers))
//
//            let ((evictfrom, evictionCandidates), rest) =
//                (List.head sortedExceptSinglesAndAllEvicted,
//                 List.skip 1 sortedExceptSinglesAndAllEvicted
//                 |> List.map (fun (k, v) -> v)
//                 |> List.concat
//                 |> Set.ofList)
//
//            let evict =
//                Set.ofList evictionCandidates
//                |> Set.intersect rest
//                |> Seq.tryHead
//
//            match evict with
//            | Some ev ->
//                Map.toList map
//                |> List.map (fun (k, v) ->
//                    if k = evictfrom then
//                        (k, v)
//                    else
//                        (k,
//                         List.map (fun num -> if num = ev then None else Some num) v
//                         |> List.choose id))
//                |> Map.ofList
//                |> fun newMap -> tryToFindExactFields newMap (ev :: evicted)
//            | None ->
//                printfn "Nothing found to evict... If this appears continiously its an endless loop..."
//
//                printfn
//                    "Evictions: %A, map: %A"
//                    evicted
//                    (map
//                     |> Map.toList
//                     |> List.filter (fun (k, v) -> k.Name.StartsWith("departure")))
//
//                tryToFindExactFields map evicted

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

crackFields rules validTickets |> printfn "%A"
