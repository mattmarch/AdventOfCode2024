module AdventOfCode2024.Day19

open System.Collections.Generic
open AdventOfCode2024.Common

let parseInput input =
    let towelList, patterns = input |> splitBy "\n\n" |> unpack2
    towelList |> splitBy ", ", patterns |> splitBy "\n"

let canMatchPattern (towels: string list) (pattern: string) =
    let memoDict = Dictionary<string, bool>()
    let memoized = memoize memoDict

    let rec canMatchRemaining (remainingPattern: string) =
        let possibleNextTowels = towels |> List.filter remainingPattern.StartsWith

        possibleNextTowels
        |> List.exists (fun towel ->
            match remainingPattern.[towel.Length ..] with
            | "" -> true
            | remaining -> memoized canMatchRemaining remaining)

    canMatchRemaining pattern


let countPossiblePatterns (towels: string list) (pattern: string) =
    let memoDict = Dictionary<string, int64>()
    let memoized = memoize memoDict

    let rec countInRemaining (remainingPattern: string) =
        let possibleNextTowels = towels |> List.filter remainingPattern.StartsWith

        possibleNextTowels
        |> List.sumBy (fun towel ->
            match remainingPattern.[towel.Length ..] with
            | "" -> 1L
            | remaining -> memoized countInRemaining remaining)

    countInRemaining pattern

let solve () =
    let towels, patterns = readAllText "19" |> parseInput
    let result = patterns |> List.filter (canMatchPattern towels) |> List.length
    printfn $"Day 19 - Part 1: %d{result}"

    let result2 = patterns |> List.sumBy (countPossiblePatterns towels)
    printfn $"Day 19 - Part 2: %d{result2}"
