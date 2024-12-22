module AdventOfCode2024.Day22

open AdventOfCode2024.Common

let mix secretNumber result : int64 = secretNumber ^^^ result

let prune secretNumber : int64 = secretNumber % 16777216L

let applyStep operation secretNumber =
    secretNumber |> operation |> mix secretNumber |> prune

let evolve secretNumber =
    secretNumber
    |> applyStep ((*) 64L)
    |> applyStep (fun n -> n / 32L)
    |> applyStep ((*) 2048L)

let rec evolveNTimes n secretNumber =
    match n with
    | 0 -> secretNumber
    | _ -> evolveNTimes (n - 1) (evolve secretNumber)

let solve () =
    let inputs = readLines "22" |> Seq.map int64

    let result = inputs |> Seq.sumBy (evolveNTimes 2000)
    printfn $"Day 22 - Part 1: %d{result}"
