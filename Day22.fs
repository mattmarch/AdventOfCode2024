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

let getPrices startSecret =
    Seq.unfold (fun secret -> Some(secret % 10L, evolve secret)) startSecret
    |> Seq.take 2000

let getGetPricesForSequence prices =
    prices
    |> Seq.windowed 5
    |> Seq.map (function
        | [| a; b; c; d; e |] -> ([ e - d; d - c; c - b; b - a ], e)
        | _ -> failwithf "Invalid windowed output")
    |> Seq.groupBy fst
    |> Seq.map (fun (sequence, group) -> sequence, group |> Seq.head |> snd)
    |> Map.ofSeq

let combinePricesPerSequence a b =
    Map.fold
        (fun acc key value ->
            match acc |> Map.tryFind key with
            | Some v -> acc |> Map.add key (v + value)
            | None -> acc |> Map.add key value)
        a
        b

let findBestSequence secretNumbers =
    secretNumbers
    |> Seq.map (getPrices >> getGetPricesForSequence)
    |> Seq.reduce combinePricesPerSequence
    |> Map.values
    |> Seq.max

let solve () =
    let inputs = readLines "22" |> Seq.map int64

    let result = inputs |> Seq.sumBy (evolveNTimes 2000)
    printfn $"Day 22 - Part 1: %d{result}"

    let result2 = findBestSequence inputs
    printfn $"Day 22 - Part 2: %d{result2}"
