module AdventOfCode2024.Day04

open Common

type WordSearch = Map<Vec2d, char>

let parseFile (input: string seq) : WordSearch =
    input
    |> Seq.mapi (fun y row -> row |> Seq.mapi (fun x c -> ((x, y), c)))
    |> Seq.concat
    |> Map.ofSeq

let allDirections =
    [ (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1) ]

let checkRestOfWord (wordSearch: WordSearch) (start: Vec2d) (direction: Vec2d) (word: char list) =
    let rec checkRestOfWord' coord word =
        match word with
        | [] -> true
        | nextChar :: remainingWord ->
            let nextCoord = addVec2d coord direction

            match Map.tryFind nextCoord wordSearch with
            | Some foundChar when nextChar = foundChar -> checkRestOfWord' nextCoord remainingWord
            | _ -> false

    checkRestOfWord' start word

let findStartingPoints (wordSearch: WordSearch) (firstChar: char) =
    wordSearch |> Map.filter (fun _ c -> c = firstChar) |> Map.keys

let isMs (c1, c2) =
    match c1, c2 with
    | 'M', 'S' -> true
    | 'S', 'M' -> true
    | _ -> false

let checkForCross (wordSearch: WordSearch) (coord: Vec2d) =
    let lettersOnDiagonal =
        [ (-1, -1); (1, -1); (1, 1); (-1, 1) ]
        |> Seq.map (addVec2d coord)
        |> Seq.map (fun c -> Map.tryFind c wordSearch)
        |> Seq.toList

    match lettersOnDiagonal with
    | [ Some tl; Some tr; Some br; Some bl ] when isMs (tl, br) && isMs (tr, bl) -> true
    | _ -> false

let solve () =
    let wordSearch = readLines "04" |> parseFile
    let firstChar = 'X'
    let remainingWord = "MAS" |> Seq.toList

    let result1 =
        findStartingPoints wordSearch firstChar
        |> Seq.collect (fun start -> allDirections |> Seq.map (fun d -> (start, d)))
        |> Seq.filter (fun (start, direction) -> checkRestOfWord wordSearch start direction remainingWord)
        |> Seq.length

    printfn $"Day 04 - Part 1: %d{result1}"

    let result2 =
        findStartingPoints wordSearch 'A'
        |> Seq.filter (checkForCross wordSearch)
        |> Seq.length

    printfn $"Day 04 - Part 2: %d{result2}"
