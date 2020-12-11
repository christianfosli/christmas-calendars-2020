open System.IO

let shiftChar shift (c: char) =
    // Non-intelegent shift working only for lower-case alphabetical chars
    let ascii = int c
    let alphabetIndex = ascii - 97
    let shiftedAlphabetIndex = (alphabetIndex + shift) % 26
    let shiftedAscii = shiftedAlphabetIndex + 97 |> char
    shiftedAscii

let caesarShift shift (input: string) =
    Seq.map (fun c -> shiftChar shift c) input
    |> Seq.map string
    |> Seq.reduce (+)

let add (str1: string) (str2: string) =
    Seq.zip str1 str2
    |> Seq.map (fun (c1, c2) -> shiftChar (int c1 - 97) c2)
    |> Seq.map string
    |> Seq.reduce (+)

let nextRow (row: string) =
    row.Substring(1)
    |> caesarShift 1
    |> fun x -> add row x

let rec allRows hint =
    if String.length hint <= 1 then [ hint ] else hint :: allRows (nextRow hint)

let safeSubstring i len (str: string) =
    try
        str.Substring(i, len)
    with _ -> ""

let guessPassword (hint: string) =
    allRows hint
    |> List.fold (fun acc el ->
        match acc with
        | [] -> Seq.map (fun c -> string c) el |> List.ofSeq
        | l -> List.mapi (fun i x -> x + (safeSubstring i 1 el)) l) []

File.ReadLines "hint.txt"
|> Seq.find (fun hint ->
             guessPassword hint
             |> List.exists (fun p -> p.Contains("eamqia")))
|> printfn "%s"
