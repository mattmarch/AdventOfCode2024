module AdventOfCode2024.Day24

open System.Collections.Generic
open AdventOfCode2024.Common

let parseWireValue wireValueLine =
    let name, value = wireValueLine |> splitBy ": " |> unpack2
    name, int value

let parseGate gate =
    match gate with
    | "XOR" -> (^^^)
    | "AND" -> (&&&)
    | "OR" -> (|||)
    | _ -> failwithf $"Unexpected gate {gate}"

let parseGateLine gateInput =
    match gateInput |> splitBy " " with
    | [ w1; gate; w2; "->"; output ] -> (output, (parseGate gate, (w1, w2)))
    | _ -> failwithf $"Unexpected gate input line {gateInput}"


let parseInput input =
    let wireValueInputs, gateInputs =
        input |> splitBy "\n\n" |> List.map (splitBy "\n") |> unpack2

    (wireValueInputs |> List.map parseWireValue, gateInputs |> List.map parseGateLine |> Map.ofList)

let rec getWireValue cache wireMap wireName =
    let cachedGetWireValue = memoize cache (getWireValue cache wireMap)
    let operation, (input1Name, input2Name) = wireMap |> Map.find wireName
    operation (cachedGetWireValue input1Name) (cachedGetWireValue input2Name)

let assembleNumber (zNumbers: (string * int) seq) =
    zNumbers
    |> Seq.sumBy (fun (name, v) ->
        let bitShiftAmount = int (name.Substring(1))
        (int64 v) <<< bitShiftAmount)

let solve () =
    let wireValues, wireMap = readAllText "24" |> parseInput
    let cache = Dictionary<string, int>()
    wireValues |> List.iter cache.Add

    let result =
        wireMap
        |> Map.keys
        |> Seq.filter _.StartsWith("z")
        |> Seq.map (fun i -> i, getWireValue cache wireMap i)
        |> assembleNumber

    printfn $"Day 24 - Part 1: %d{result}"
