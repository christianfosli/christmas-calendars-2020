open System.IO

let draw deck = List.head deck, List.skip 1 deck

let rec playCombat deck1 deck2 =
    if List.length deck1 = 0 || List.length deck2 = 0 then
        (deck1, deck2)
    else
        let (card1, deck1) = draw deck1
        let (card2, deck2) = draw deck2

        if card1 > card2
        then playCombat (deck1 @ card1 :: [ card2 ]) deck2
        else playCombat deck1 (deck2 @ card2 :: [ card1 ])

let findWinner (deck1, deck2) =
    match (deck1, deck2) with
    | ([], winner2) -> winner2
    | (winner1, []) -> winner1
    | _ -> failwith "No-one won :confused:"

let score deck =
    deck
    |> List.rev
    |> List.mapi (fun i el -> (el * (i + 1)))
    |> List.sum

let parseDeck (deck: string) =
    deck.Trim().Split("\n")
    |> Array.skip 1
    |> Array.map int
    |> List.ofArray

File.ReadAllText "input.txt"
|> fun text -> text.Split("\n\n")
|> fun decks -> (parseDeck decks.[0], parseDeck decks.[1])
|> fun (deck1, deck2) -> playCombat deck1 deck2
|> findWinner
|> score
|> printfn "Winners score: %i"


// Tests
playCombat [ 9; 2; 6; 3; 1 ] [
    5
    8
    4
    7
    10
]
|> findWinner
|> score
|> fun s ->
    match s with
    | 306 -> ()
    | err -> failwithf "Error: expected 306 but got %i" err
