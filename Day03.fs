module AdventOfCode2024.Day03

open System.Text.RegularExpressions
open AdventOfCode2024.Common

let mulRegex = Regex "mul\\((\d+),(\d+)\\)"
let disabledSectionRegex = Regex "don't\\(\\)[\s\S]+?(do\\(\\)|$)"

let part1 text =
    text
    |> mulRegex.Matches
    |> Seq.map (fun m -> (int64 m.Groups[1].Value) * (int64 m.Groups[2].Value))
    |> Seq.sum

let solve () =
    let input = readAllText "03"
    let result1 = part1 input
    printfn $"Day 03 - Part 1: %d{result1}"

    let reducedInputMatches = disabledSectionRegex.Replace(input, "XX")
    let result2 = part1 reducedInputMatches
    printfn $"Day 03 - Part 2: %d{result2}"
