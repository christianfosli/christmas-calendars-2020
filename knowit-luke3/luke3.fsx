#r "nuget: FSharp.Data"
open FSharp.Data

let containsWord (matrix: char list list) (word: string): bool =
    let word = Seq.ofArray (word.ToCharArray())
    matrix |> List.exists (fun row -> List.contains word row)
    // aarghghghg this is a mess

let notHere (matrix: char list list) (words: string list) =
    words
    |> List.filter (fun word -> not (containsWord matrix word))
    |> List.sort

let asMatrix (text: string) =
    text
    |> fun text -> text.Split "\n"
    |> List.ofArray
    |> List.map (fun line -> List.ofArray (line.ToCharArray()))


module Main =
    let matrix =
        Http.RequestString
            "https://gist.githubusercontent.com/knowitkodekalender/d277d4f01a9fe10f7c1d92e2d17f1b31/raw/49da54e4372a83f4fc11d7137f19fc8b4c58bda6/matrix.txt"
        |> asMatrix

    let words =
        Http.RequestString
            "https://gist.githubusercontent.com/knowitkodekalender/9e1ba20cd879b0c6d7af4ccfe8a87a19/raw/b19ae9548a33a825e2275d0283986070b9b7a126/wordlist.txt"
        |> fun res -> res.Split "\n"
        |> List.ofArray

    notHere matrix words
    |> String.concat ","
    |> printfn "%A"

module Test =
    let words =
        [ "kakao"
          "kriminalroman"
          "kvikklunch"
          "kylling"
          "langfredag"
          "langrennski"
          "palmesøndag"
          "påskeegg"
          "smågodt"
          "solvegg"
          "yatzy" ]

    let matrix =
        "vlzzrkytiempkxg
wkuwuuniimpuzka
ufrazcavumtagod
ooscwzmvscdngwe
lskokdozvxvecer
povfkarkkmgoovf
vlirgaldqisatsg
pvknfgayzgqkcnn
iekozvnabdyapva
zgllegiizobkyjl
lgukatmaltamzba
lvnrvdizullcvsx
oscponrepvyatzy
rbhovtkpfljkihq
wjssiksnnergnal"
        |> asMatrix

    match notHere matrix words with
    | [ "palmesøndag"; "påskeegg"; "smågodt" ] -> ()
    | wrong -> failwithf "expected %A but got %A" [ "palmesøndag"; "påskeegg"; "smågodt" ] wrong

open Test
open Main
