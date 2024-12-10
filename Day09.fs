module AdventOfCode2024.Day09

open AdventOfCode2024.Common

type Block =
    | Empty
    | Used of int


let parseInput input =
    input
    |> Seq.map charToInt
    |> Seq.chunkBySize 2
    |> Seq.indexed
    |> Seq.map (function
        | i, [| fileSize; emptySize |] -> Seq.append (Seq.replicate fileSize (Used i)) (Seq.replicate emptySize Empty)
        | i, [| fileSize |] -> Seq.replicate fileSize (Used i)
        | _ -> failwithf "Unexpected output from chunkBy")
    |> Seq.concat

let runCompactor (blocks: Block list) =
    let reversedBlocks = blocks |> List.rev

    let usedBlocks = blocks |> List.filter _.IsUsed |> List.length

    let initialState = blocks, reversedBlocks

    initialState
    |> Seq.unfold (fun (blocks, reversedBlocks) ->
        match blocks with
        | [] -> None
        | Used i :: rest -> Some(i, (rest, reversedBlocks))
        | Empty :: rest ->
            match reversedBlocks |> List.skipWhile _.IsEmpty with
            | Used i :: reversedRest -> Some(i, (rest, reversedRest))
            | _ -> failwithf "Run out of input")
    |> Seq.take usedBlocks

let calculateChecksum blocks =
    blocks |> Seq.mapi (fun i b -> int64 b * int64 i) |> Seq.sum

let solve () =
    let input = readAllText "09" |> parseInput |> Seq.toList
    let result = input |> runCompactor |> calculateChecksum

    printfn $"Day 09 - Part 1: %d{result}"
