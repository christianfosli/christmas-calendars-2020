let isPrime n =
    // stolen from https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/sequences#examples
    let rec check i =
        i > n / 2 || (n % i <> 0 && check (i + 1))

    check 2

let rec getGoodPresents (presents: int list) =
    let good =
        presents
        |> List.takeWhile (fun n -> not (Seq.contains '7' (string n)))

    let bad, rest =
        presents
        |> List.skip (List.length good)
        |> fun lst -> (List.head lst, List.skip 1 lst)

    let nearestPrime =
        [ 0 .. bad ] |> List.rev |> List.find isPrime

    if nearestPrime < List.length rest then
        good
        @ getGoodPresents (rest |> List.skip nearestPrime)
    else
        good

[ 0 .. (5_433_000 - 1) ]
|> getGoodPresents
|> List.length
|> printfn "%A"

// Tests
match ([ 0 .. 9 ] |> getGoodPresents) with
| [ 0; 1; 2; 3; 4; 5; 6 ] -> ()
| wrong -> failwithf "got %A" wrong

match ([ 0 .. 19 ] |> getGoodPresents) with
| [ 0; 1; 2; 3; 4; 5; 6; 15; 16 ] -> ()
| wrong -> failwithf "got %A" wrong
