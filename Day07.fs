module AdventOfCode2024.Day07

open AdventOfCode2024.Common

let parseLine line =
    let total, numbers = line |> splitBy ": " |> unpack2
    int64 total, numbers |> splitBy " " |> List.map int64

let isEquationPossible operators total numbers =
    let rec testPossible runningTotal remainingNumbers =
        match remainingNumbers with
        | [] -> total = runningTotal
        | nextNum :: rest ->
            operators
            |> List.exists (fun operator -> testPossible (operator runningTotal nextNum) rest)

    match numbers with
    | [] -> failwithf $"Empty list of numbers provided"
    | head :: tail -> testPossible head tail

let concatNums n1 n2 = (string n1 + string n2) |> int64

let solve () =
    let input = readLines "07" |> Seq.map parseLine

    let result =
        input
        |> Seq.filter (fun (total, numbers) -> isEquationPossible [ (+); (*) ] total numbers)
        |> Seq.sumBy fst

    printfn $"Day 07 - Part 1: %d{result}"

    let result =
        input
        |> Seq.filter (fun (total, numbers) -> isEquationPossible [ (+); (*); concatNums ] total numbers)
        |> Seq.sumBy fst

    printfn $"Day 07 - Part 2: %d{result}"
