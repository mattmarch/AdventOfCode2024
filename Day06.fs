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
    let initialDirection = (0, -1)

    (initialDirection, start)
    |> Seq.unfold (fun (direction, from) ->
        if outsideBounds bounds from then
            None
        else
            let nextStep = addVec2d from direction

            if grid |> Set.contains nextStep then
                Some((direction, from), (nextDirection direction, from))
            else
                Some((direction, from), (direction, nextStep)))

// Super nasty (and slow) check for a loop by assuming all non-loop paths have length < 10000
// (It is faster than keeping track of a Set of all visited squares though!)
let maxLength = 10000

let hasLoop bounds start obstacles =
    traverseGrid bounds obstacles start |> Seq.truncate maxLength |> Seq.length = maxLength


let solve () =
    let input = readLines "06"
    let start, obstacles = parseGrid input
    let ySize = input |> Seq.length
    let xSize = input |> Seq.head |> String.length
    let traversedSquares = traverseGrid (xSize, ySize) obstacles start
    let squaresVisited = traversedSquares |> Seq.map snd |> Seq.distinct
    let result = Seq.length squaresVisited

    printfn $"Day 06 - Part 1: %d{result}"

    let result2 =
        squaresVisited
        |> Seq.filter (fun sq -> hasLoop (xSize, ySize) start (obstacles |> Set.add sq))
        |> Seq.length

    printfn $"Day 06 - Part 2: %d{result2}"
