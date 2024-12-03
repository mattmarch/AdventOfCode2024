module AdventOfCode2024.Day02

open AdventOfCode2024.Common

let parseLine line = line |> splitBy " " |> List.map int

let differences (levels: int list) =
    levels
    |> Seq.windowed 2
    |> Seq.map (function
        | [| a; b |] -> b - a
        | _ -> failwith "Invalid windowed output")

let isSafe (report: int list) =
    let differences = differences report
    let anyGreaterThan3 = differences |> Seq.map abs |> Seq.exists ((<) 3)
    let allPositive = differences |> Seq.forall ((<) 0)
    let allNegative = differences |> Seq.forall ((>) 0)
    not anyGreaterThan3 && (allPositive || allNegative)

let allRemovedPossibilities (report: int list) =
    report |> Seq.mapi (fun i _ -> report |> List.removeAt i)

let isSafeWithOneRemoved (report: int list) =
    isSafe report || report |> allRemovedPossibilities |> Seq.exists isSafe

let solve () =
    let inputLines = readLines "02"
    let safeReports = inputLines |> Seq.map parseLine |> Seq.filter isSafe |> Seq.length
    printfn $"Day 02 - Part 1: %d{safeReports}"

    let safeReportsB =
        inputLines |> Seq.map parseLine |> Seq.filter isSafeWithOneRemoved |> Seq.length

    printfn $"Day 02 - Part 2: %d{safeReportsB}"
