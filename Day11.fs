module AdventOfCode2024.Day11

open AdventOfCode2024.Common

let parseInput = splitBy " " >> List.map int64

let (|EvenDigits|_|) (number: int64) =
    let numberAsString = string number
    let digits = String.length numberAsString

    if digits % 2 = 0 then
        let firstHalf = numberAsString.Substring(0, digits / 2) |> int64
        let secondHalf = numberAsString.Substring(digits / 2) |> int64
        Some(firstHalf, secondHalf)
    else
        None

let runRules (stone: int64) =
    match stone with
    | 0L -> [ 1L ]
    | EvenDigits(firstHalf, secondHalf) -> [ firstHalf; secondHalf ]
    | _ -> [ stone * 2024L ]

let runBlinks numBlinks (stones: int64 list) =
    let stoneCounts =
        stones |> List.countBy id |> List.map (fun (stone, count) -> stone, int64 count)

    let rec runRemainingBlinks stoneCounts remainingBlinks =
        let updatedCounts =
            stoneCounts
            |> List.collect (fun (stone, count) -> runRules stone |> List.map (fun newStone -> newStone, count))
            |> List.groupBy fst
            |> List.map (fun (stone, counts) -> stone, counts |> List.sumBy snd)

        if remainingBlinks > 1 then
            runRemainingBlinks updatedCounts (remainingBlinks - 1)
        else
            updatedCounts

    runRemainingBlinks stoneCounts numBlinks

let countStones (stones: (int64 * int64) list) = stones |> List.sumBy snd

let solve () =
    let initialStones = readAllText "11" |> parseInput
    let result = runBlinks 25 initialStones |> countStones
    printfn $"Day 11 - Part 1: %d{result}"
    let result2 = runBlinks 75 initialStones |> countStones
    printfn $"Day 11 - Part 2: %d{result2}"
