open System.IO

// customs forms
// -> questions are represented by single letters
// -> groups of people are separated by blank lines
// -> persons within a group are separated by newlines

let customDeclarationsByGroup =
    File.ReadAllText "puzzle.txt"
    |> fun text -> text.Trim().Split "\n\n"

// part 1
let countQuestionsAnswered (answers: string) =
    answers.Replace("\n", "")
    |> Set.ofSeq
    |> Set.count

customDeclarationsByGroup
|> Array.sumBy countQuestionsAnswered
|> printfn "Sum of 'yes'-questions answered by anyone per group: %i"

// part 2
let countQuestionsAnsweredByEveryone (answers: string) =
    answers.Split "\n"
    |> Seq.map Set.ofSeq
    |> Set.intersectMany
    |> Set.count

customDeclarationsByGroup
|> Array.sumBy countQuestionsAnsweredByEveryone
|> printfn "Sum of 'yes'-questions answered by everyone per group: %i"
