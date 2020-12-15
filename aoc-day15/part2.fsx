// Map based version because the list got too big and became unusably slow
let rec memoryGame turn limit num spokenNums =
    let newSpokenNums = Map.add num turn spokenNums

    if turn = limit then
        // Game finished
        printfn "Last number (%i) was %i" limit num
        newSpokenNums
    else
        match Map.tryFind num spokenNums with
        | Some lastTurn -> turn - lastTurn
        | None -> 0
        |> fun nextNum -> memoryGame (turn + 1) limit nextNum newSpokenNums

let startingNumbers = [ 14; 3; 1; 0; 9; 5 ]

let startingNumbersExceptLast =
    startingNumbers.[0..List.length startingNumbers - 2]
    |> List.mapi (fun i el -> (el, i + 1))
    |> Map.ofList

let last = List.last startingNumbers

memoryGame (List.length startingNumbers) 30_000_000 last startingNumbersExceptLast
