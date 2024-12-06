module AdventOfCode2024.Day06

open AdventOfCode2024.Common

type Symbol =
    | Guard
    | Obstacle

let parseLine (y: int) (line: string) =
    line
    |> Seq.indexed
    |> Seq.choose (function
        | (x, '#') -> Some(Obstacle, (x, y))
        | (x, '^') -> Some(Guard, (x, y))
        | _ -> None)

let parseGrid (input: string seq) =
    let symbolsInGrid = input |> Seq.mapi parseLine |> Seq.concat

    let guardPosition =
        symbolsInGrid
        |> Seq.choose (function
            | (Guard, coord) -> Some coord
            | _ -> None)
        |> Seq.head

    let obstacles =
        symbolsInGrid
        |> Seq.choose (function
            | (Obstacle, coord) -> Some coord
            | _ -> None)
        |> Set.ofSeq

    guardPosition, obstacles

let nextDirection =
    function
    | (0, -1) -> (1, 0)
    | (1, 0) -> (0, 1)
    | (0, 1) -> (-1, 0)
    | (-1, 0) -> (0, -1)
    | d -> failwithf $"Invalid direction {d}"

let outsideBounds (xLim, yLim) (x, y) =
    x >= xLim || x < 0 || y >= yLim || y < 0

let traverseGrid bounds grid start =
    let rec takeStep from direction =
        let nextStep = addVec2d from direction

        if outsideBounds bounds nextStep then
            []
        else
            match grid |> Set.contains nextStep with
            | true -> takeStep from (nextDirection direction)
            | false -> nextStep :: takeStep nextStep direction

    start :: takeStep start (0, -1)

let solve () =
    let input = readLines "06"
    let start, obstacles = parseGrid input
    let ySize = input |> Seq.length
    let xSize = input |> Seq.head |> String.length
    let traversedSquares = traverseGrid (xSize, ySize) obstacles start
    let result = traversedSquares |> List.distinct |> List.length

    printfn $"Day 06 - Part 1: %d{result}"
