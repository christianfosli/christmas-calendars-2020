let isPrime n =
    let rec check i =
        i > n / 2 || (n % i <> 0 && check (i + 1))

    check 2

let rec getGoodPresents (presents: seq<int>) =
    let good =
        presents
        |> Seq.takeWhile (fun n -> not (Seq.contains '7' (string n)))
        |> List.ofSeq

    let rest = presents |> Seq.skip (Seq.length good)

    let nearestPrime =
        [ 0 .. (Seq.head rest) ]
        |> Seq.rev
        |> Seq.find isPrime

    if nearestPrime < Seq.length rest then
        good
        @ getGoodPresents (rest |> Seq.skip 1 |> Seq.skip nearestPrime)
    else
        good

[ 0 .. (5_433_000 - 1) ]
|> getGoodPresents
|> Seq.length
|> printfn "%A"

// Tests
match ([ 0 .. 9 ] |> getGoodPresents) with
| [ 0; 1; 2; 3; 4; 5; 6 ] -> ()
| wrong -> failwithf "got %A" wrong

match ([ 0 .. 19 ] |> getGoodPresents) with
| [0; 1; 2; 3; 4; 5; 6; 15; 16 ] -> ()
| wrong -> failwithf "got %A" wrong
