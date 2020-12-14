open System
open System.IO
open System.Text.RegularExpressions

module Bitmask =
    let combinations (mask: string) =
        let allCombinations lst =
            let rec comb accLst elemLst =
            match elemLst with
            | head::tail ->
                let next = [h]::List.map(fun el -> head::el) accLst @ accLst
                comb next tail
            | _ -> accLlst
        comb [] lst

        let rec wildcardIndexes (mask: string) offset = 
            match Seq.tryIndex (fun x -> x = 'X') mask with
            | None -> []
            | i -> i+offset :: wildcardIndexes mask.Substring(i) offset+i
        
        // TODO: return a list of memory addresses

    let apply (value: int64) (mask: string) =
        let setBits mask value =
            mask
            |> Seq.rev
            |> Seq.mapi (fun i el ->
                match el with
                | '1' -> pown 2L i
                | _ -> 0L)
            |> Seq.sum
            |> fun x -> value ||| x

        let unsetBits mask value =
            mask
            |> Seq.rev
            |> Seq.mapi (fun i el ->
                match el with
                | '0' -> 0L
                | _ -> pown 2L i)
            |> Seq.sum
            |> fun x -> value &&& x

        value |> setBits mask |> unsetBits mask

module Memory =
    // not handling edge-cases/writing to edges of address space
    let write (value: int64) (offset: int) (mask: string) (mem: int64 list) =
        mem.[0..offset - 1]
        @ (Bitmask.apply value mask) :: mem.[offset + 1..]

// Part 1 -- TODO Replace with part 2
type Parsed =
    | Mask of string
    | SetMem of (int * int64)

let parse (line: string) =
    match line.Substring(0, 3) with
    | "mas" -> line.Split("=").[1].Trim() |> Mask
    | "mem" ->
        line.Split("=")
        |> fun l -> (l.[0], l.[1])
        |> fun (offset, value) -> (Regex("\\d+").Match(offset).Value, value.Trim())
        |> fun (offset, value) -> SetMem(int offset, int64 value)
    | _ -> failwith "ParseError: Unexpected format"

File.ReadLines "input.txt"
|> Seq.map parse
|> Seq.fold (fun (mem, mask) el ->
    match el with
    | Mask m -> (mem, m)
    | SetMem (offset, value) -> (Memory.write value offset mask mem, mask)) (Array.zeroCreate 65536 |> List.ofArray, "")
|> fun (mem, _) -> List.sum mem
|> printfn "%i"