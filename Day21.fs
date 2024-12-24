module AdventOfCode2024.Day21

open System.Collections.Generic
open AdventOfCode2024.Common

let numericButtonPosition button : Vec2d =
    match button with
    | '7' -> (0, 0)
    | '8' -> (1, 0)
    | '9' -> (2, 0)
    | '4' -> (0, 1)
    | '5' -> (1, 1)
    | '6' -> (2, 1)
    | '1' -> (0, 2)
    | '2' -> (1, 2)
    | '3' -> (2, 2)
    | '0' -> (1, 3)
    | 'A' -> (2, 3)
    | _ -> failwithf $"Unexpected numeric button {button}"

type DirectionButton =
    | Up
    | Down
    | Left
    | Right
    | A

let directionButtonPosition button : Vec2d =
    match button with
    | Up -> (1, 0)
    | A -> (2, 0)
    | Left -> (0, 1)
    | Down -> (1, 1)
    | Right -> (2, 1)

let getDirectionPressesToNextPosition fromPosition toPosition =
    let x, y = subVec2d toPosition fromPosition

    let verticalDir = if y > 0 then Up else Down
    let horizontalDir = if x > 0 then Left else Right

    List.replicate (abs y) verticalDir
    @ List.replicate (abs x) horizontalDir
    @ [ A ]

let getDirectionPressesToNextNumber fromButton toButton =
    let fromPos = numericButtonPosition fromButton
    let toPos = numericButtonPosition toButton
    let x, y = subVec2d toPos fromPos

    match fromPos, toPos with
    | (0, _), (_, 3) ->
        // need to move right before down to avoid panic square
        List.replicate x Right @ List.replicate y Down @ [ A ]
    | (_, 3), (0, _) ->
        // need to move up before left to avoid panic square
        List.replicate (-y) Up @ List.replicate (-x) Left @ [ A ]
    | _ ->
        // else prioritise Left > Down > Right > Up
        List.replicate (if x < 0 then -x else 0) Left
        @ List.replicate (if y > 0 then y else 0) Down
        @ List.replicate (if x > 0 then x else 0) Right
        @ List.replicate (if y < 0 then -y else 0) Up
        @ [ A ]

let getDirectionPressesForCode (code: string) =
    Seq.pairwise $"A{code}"
    |> Seq.collect (fun (fromButton, toButton) -> getDirectionPressesToNextNumber fromButton toButton)
    |> Seq.toList

let getDirectionPressesToNextDirection fromDirection toDirection =
    let x, y =
        subVec2d (directionButtonPosition toDirection) (directionButtonPosition fromDirection)

    if toDirection = Left then
        // Going to "<" need to move down first to avoid panic square
        List.replicate y Down @ List.replicate (-x) Left @ [ A ]
    else
        // else prioritise Left > Down > Right > Up
        List.replicate (if x < 0 then -x else 0) Left
        @ List.replicate (if y > 0 then y else 0) Down
        @ List.replicate (if x > 0 then x else 0) Right
        @ List.replicate (if y < 0 then -y else 0) Up
        @ [ A ]

let getDirectionPressesForDirectionSequence directions =
    List.append [ A ] directions
    |> List.pairwise
    |> List.collect (fun (fromDirection, toDirection) -> getDirectionPressesToNextDirection fromDirection toDirection)

let directionSequenceCache =
    Dictionary<DirectionButton list, DirectionButton list>()

let cachedGetDirectionPressesForDirectionSequence =
    memoize directionSequenceCache getDirectionPressesForDirectionSequence

let rec translateButtonPressesNRobots n directions =
    match n with
    | 0 -> directions
    | _ -> translateButtonPressesNRobots (n - 1) (cachedGetDirectionPressesForDirectionSequence directions)

let getComplexity n code =
    let buttonPresses =
        code |> getDirectionPressesForCode |> translateButtonPressesNRobots n

    (code.Replace("A", "") |> int) * (Seq.length buttonPresses)

let solve () =
    let input = readLines "21"
    let result = input |> Seq.sumBy (getComplexity 2)

    printfn $"Day 21 - Part 1: %d{result}"

    let result2 = input |> Seq.sumBy (getComplexity 15)
    printfn $"Day 21 - Part 2: %d{result2}"
