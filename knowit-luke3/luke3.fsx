//#r "nuget: FSharp.Data"
//open FSharp.Data
open Microsoft.FSharp.Collections

// --- Didn't finish this one in time --- //

let asMatrix (text: string) =
    text
    |> fun text -> text.Split "\n"
    |> List.ofArray
    |> List.map (fun line -> List.ofArray (line.ToCharArray()))
    |> array2D

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

let found =
    words
    |> Seq.filter (fun word ->
        let w = List.ofArray (word.ToCharArray())

        if matrix.[1.., *]
           |> Seq.cast<'T>
           |> Seq.exists (fun row ->
               Seq.contains w row
               || (Seq.rev row |> Seq.contains w)) then
            true
        elif matrix.[*, 1..]
             |> Seq.cast<'T>
             |> Seq.exists (fun col ->
                 Seq.contains w col
                 || (Seq.rev col |> Seq.contains w)) then
            true
        else
            // TODO: Diag
            false)
    |> Set.ofSeq

match List.ofSeq (found - Set.ofList words) with
| [ "palmesøndag"; "påskeegg"; "smågodt" ] -> ()
| wrong -> failwithf "expected %A but got %A" [ "palmesøndag"; "påskeegg"; "smågodt" ] wrong
