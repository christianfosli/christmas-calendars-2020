// The starting numbers must be ordered such that recently spoken numbers come first
// because the fsharp list implementation prefers access to head
let rec memoryGame limit startingNumbers =
    if List.length startingNumbers = limit then
        // game ends
        startingNumbers
    else
        let last, earlier =
            List.head startingNumbers, List.skip 1 startingNumbers

        match List.tryFindIndex (fun x -> x = last) earlier with
        | Some i -> i + 1 :: startingNumbers
        | None -> 0 :: startingNumbers
        |> memoryGame limit

// part 1
let startingNumbers = [ 14; 3; 1; 0; 9; 5 ] |> List.rev

memoryGame 2020 startingNumbers
|> List.head
|> printfn "2020'th number: %i"
