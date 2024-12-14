module AdventOfCode2024.Day13

open System.Text.RegularExpressions
open AdventOfCode2024.Common

type MachineDescription =
    { ButtonA: Vec2dL
      ButtonB: Vec2dL
      Prize: Vec2dL }

let buttonRegex = Regex "Button (?:A|B): X\\+(\d+), Y\\+(\d+)"
let prizeRegex = Regex "Prize: X=(\d+), Y=(\d+)"

let parseLine (regex: Regex) line =
    let m = regex.Match line
    let x = int64 m.Groups.[1].Value
    let y = int64 m.Groups.[2].Value
    (x, y)

let parseMachine input =
    match input |> splitBy "\n" with
    | [ buttonALine; buttonBLine; prizeLine ] ->
        { ButtonA = parseLine buttonRegex buttonALine
          ButtonB = parseLine buttonRegex buttonBLine
          Prize = parseLine prizeRegex prizeLine }
    | l -> failwith $"Invalid input, found ${List.length l} lines, expected 3"

let parseInput = splitBy "\n\n" >> Seq.map parseMachine

let quotientIfDivisible a b =
    if b <> 0L && a % b = 0L then Some(a / b) else None

// Solving: A (xa, ya) + B (xb, yb) = (xt, yt)
let getButtonPresses
    { ButtonA = (xa, ya)
      ButtonB = (xb, yb)
      Prize = (xt, yt) }
    =
    let a = quotientIfDivisible ((xt * yb) - (yt * xb)) ((xa * yb) - (xb * ya))
    let b = quotientIfDivisible ((yt * xa) - (xt * ya)) ((xa * yb) - (xb * ya))

    match a, b with
    | Some a, Some b -> Some(a, b)
    | _ -> None

let validButtonPresses presses = presses > 0L && presses < 100L

let measurementError = 10000000000000L

let solve () =
    let input = readAllText "13" |> parseInput

    let tokens =
        input
        |> Seq.choose getButtonPresses
        |> Seq.filter (fun (a, b) -> validButtonPresses a && validButtonPresses b)
        |> Seq.sumBy (fun (a, b) -> 3L * a + b)

    printfn $"Day 13 - Part 1: %d{tokens}"

    let tokensPart2 =
        input
        |> Seq.map (fun machine ->
            { machine with
                Prize = (machine.Prize |> fun (x, y) -> (x + measurementError, y + measurementError)) })
        |> Seq.choose getButtonPresses
        |> Seq.filter (fun (a, b) -> a > 0 && b > 0)
        |> Seq.sumBy (fun (a, b) -> 3L * a + b)

    printfn $"Day 13 - Part 2: %d{tokensPart2}"
