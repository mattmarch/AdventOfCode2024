module AdventOfCode2024.Day21

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
    getDirectionPressesToNextPosition (numericButtonPosition toButton) (numericButtonPosition fromButton)

let getDirectionPressesForCode (code: string) =
    Seq.pairwise $"A{code}"
    |> Seq.collect (fun (fromButton, toButton) -> getDirectionPressesToNextNumber fromButton toButton)

let getDirectionPressesToNextDirection fromDirection toDirection =
    getDirectionPressesToNextPosition (directionButtonPosition toDirection) (directionButtonPosition fromDirection)

let getDirectionPressesForDirectionSequence directions =
    Seq.append [ A ] directions
    |> Seq.pairwise
    |> Seq.collect (fun (fromDirection, toDirection) -> getDirectionPressesToNextDirection fromDirection toDirection)

let getComplexity code =
    let buttonPresses =
        code
        |> getDirectionPressesForCode
        |> getDirectionPressesForDirectionSequence
        |> getDirectionPressesForDirectionSequence

    (code.Replace("A", "") |> int) * (Seq.length buttonPresses)


let solve () =
    let result = readLines "21" |> Seq.sumBy getComplexity

    printfn $"Day 21 - Part 1: %d{result}"
