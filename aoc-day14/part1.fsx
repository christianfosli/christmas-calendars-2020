open System
open System.IO
open System.Text.RegularExpressions

module Bitmask =
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

// Part 1
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

// --- tests part 1 ---
match Bitmask.apply 0b000000000000000000000000000000001011L "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" with
| 0b000000000000000000000000000001001001L -> ()
| err -> failwithf "Bitmask applied incorrectly, got %A" (Convert.ToString(err, 2))

seq {
    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    "mem[8] = 11"
    "mem[7] = 101"
    "mem[8] = 0"
}
|> Seq.map parse
|> Seq.fold (fun (mem, mask) el ->
    match el with
    | Mask m -> (mem, m)
    | SetMem (offset, value) -> (Memory.write value offset mask mem, mask)) (Array.zeroCreate 65536 |> List.ofArray, "")
|> fun (mem, _) -> List.sum mem
|> fun ans ->
    match ans with
    | 165L -> ()
    | err -> failwithf "Expected 165 but got %i" err
