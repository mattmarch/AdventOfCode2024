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
    |> Seq.collect (function
        | i, [| fileSize; emptySize |] -> Seq.append (Seq.replicate fileSize (Used i)) (Seq.replicate emptySize Empty)
        | i, [| fileSize |] -> Seq.replicate fileSize (Used i)
        | _ -> failwithf "Unexpected output from chunkBy")

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

type EmptySpace = { Size: int }
type FileSpace = { Size: int; Id: int }

type Space =
    | Empty of EmptySpace
    | File of FileSpace

let parseInputPart2 input =
    input
    |> Seq.map charToInt
    |> Seq.chunkBySize 2
    |> Seq.indexed
    |> Seq.collect (function
        | i, [| fileSize; emptySize |] -> [ File { Size = fileSize; Id = i }; Empty { Size = emptySize } ]
        | i, [| fileSize |] -> [ File { Size = fileSize; Id = i } ]
        | _ -> failwithf "Unexpected output from chunkBy")

let runCompactorPart2 (spaces: Space list) =
    let filesToPlace =
        spaces
        |> List.choose (fun s ->
            match s with
            | File f -> Some f
            | _ -> None)
        |> List.rev

    filesToPlace
    |> List.fold
        (fun spacesState file ->
            let foundSpaceIndex =
                spacesState
                |> List.tryFindIndex (function
                    | Empty e when e.Size >= file.Size -> true
                    | _ -> false)

            let fileCurrentPosition = spacesState |> List.findIndex ((=) (File file))

            match foundSpaceIndex with
            | None -> spacesState
            | Some i when i > fileCurrentPosition -> spacesState
            | Some i ->
                let foundSpace =
                    spacesState
                    |> List.item i
                    |> function
                        | Empty e -> e
                        | _ -> failwith "Expected empty but actually file"

                let remainingSpace = foundSpace.Size - file.Size

                spacesState
                |> List.removeAt fileCurrentPosition
                |> List.insertAt fileCurrentPosition (Empty { Size = file.Size })
                |> List.removeAt i
                |> List.insertManyAt i [ File file; Empty { Size = remainingSpace } ])
        spaces

let hackPart2OutputIntoPart1ChecksumFormat spaces =
    spaces
    |> Seq.collect (function
        | File f -> Seq.replicate f.Size (f.Id)
        | Empty s -> Seq.replicate s.Size 0)

// TODO: Find the issue with and fix this function instead of the above hack!
let calculateChecksumPart2 (spaces: Space seq) : int64 =
    spaces
    |> Seq.fold
        (fun (checksum, index) space ->
            match space with
            | File f -> checksum + int64 (f.Id * index), index + f.Size
            | Empty s -> checksum, index + s.Size)
        (0L, 0)
    |> fst

let solve () =
    let input = readAllText "09" |> parseInput |> Seq.toList
    let result = input |> runCompactor |> calculateChecksum

    printfn $"Day 09 - Part 1: %d{result}"

    let inputPart2 = readAllText "09" |> parseInputPart2 |> Seq.toList

    let resultPart2 =
        inputPart2
        |> runCompactorPart2
        |> hackPart2OutputIntoPart1ChecksumFormat
        |> calculateChecksum

    printfn $"Day 09 - Part 2: %d{resultPart2}"
