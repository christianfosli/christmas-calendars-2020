open System.IO

let testcase = [ 1721; 979; 366; 299; 675; 1456 ]

let expenseReport =
    File.ReadLines "puzzle.txt"
    |> Seq.map int
    |> List.ofSeq

///<summary>
/// Find the two entries in your expense report that sum to 2020
/// What do you get if you multiply them together?
///</summary>
module Part1 =

    let rec multiplyFirstTwoEntriesThatSum2020 report =
        match report with
        | [] -> 0
        | first :: rest ->
            rest
            |> Seq.tryFind (fun x -> first + x = 2020)
            |> fun result ->
                match result with
                | Some r -> first * r
                | None -> multiplyFirstTwoEntriesThatSum2020 rest

    testcase
    |> multiplyFirstTwoEntriesThatSum2020
    |> fun ans ->
        match ans with
        | 514579 -> ()
        | wrong -> failwithf "expected %d but got %d" 514579 wrong

    expenseReport
    |> multiplyFirstTwoEntriesThatSum2020
    |> printfn "%A"


///<summary>
/// What is the product of the three entries that sum to 2020
///</summary>
/// TODO: Try to solve this is a functional way... 🙃
module Part2 =

    let multiplyFirstThreeEntriesThatSum2020 report =
        let mutable ans = 0

        for x in report do
            for y in report do
                for z in report do
                    match x + y + z with
                    | 2020 -> ans <- x * y * z
                    | _ -> ()

        ans

    testcase
    |> multiplyFirstThreeEntriesThatSum2020
    |> fun ans ->
        match ans with
        | 241861950 -> ()
        | wrong -> failwithf "expected %d but got %d" 241861950 wrong

    expenseReport
    |> multiplyFirstThreeEntriesThatSum2020
    |> printfn "%A"

open Part1
open Part2