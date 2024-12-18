module AdventOfCode2024.Day18

open AdventOfCode2024.Common

let parseLine line : Vec2d =
    line |> splitBy "," |> List.map int |> unpack2


let findValidSteps (x, y) corruptedSet (yMax, xMax) =
    [ x, y - 1; x + 1, y; x, y + 1; x - 1, y ]
    |> List.filter (fun (x, y) -> x >= 0 && x <= xMax && y >= 0 && y <= yMax)
    |> List.filter (fun p -> not (Set.contains p corruptedSet))

type ReachedPosition =
    { Position: Vec2d
      Steps: int
      Visited: bool }

let findSteps endCoord corruptedCoords =
    let corruptedSet = corruptedCoords |> Set.ofSeq

    let rec nextStep stepsSoFar =
        let possibleNextSteps = stepsSoFar |> List.filter (_.Visited >> not)

        if possibleNextSteps = [] then
            None
        else
            let fromPos = stepsSoFar |> List.filter (_.Visited >> not) |> List.minBy _.Steps

            let nextPositions =
                findValidSteps fromPos.Position corruptedSet endCoord
                |> List.filter (fun step -> not (stepsSoFar |> List.exists (fun p -> p.Position = step)))
                |> List.map (fun p ->
                    { Position = p
                      Steps = fromPos.Steps + 1
                      Visited = false })

            match nextPositions |> List.tryFind (fun p -> p.Position = endCoord) with
            | Some p -> Some p.Steps
            | None ->
                nextStep (
                    stepsSoFar @ nextPositions
                    |> List.map (fun p -> if p = fromPos then { p with Visited = true } else p)
                )

    nextStep
        [ { Position = (0, 0)
            Steps = 0
            Visited = false } ]

let binarySearchFirstImpossible endCoord steps =
    let rec binarySearch (min, max) =
        if min = max then
            min
        else
            let mid = (min + max) / 2

            match findSteps endCoord (steps |> Seq.take mid) with
            | Some _ -> binarySearch (mid + 1, max)
            | None -> binarySearch (min, mid)

    binarySearch (0, steps |> Seq.length)

let solve () =
    let input = readLines "18" |> Seq.map parseLine

    match input |> Seq.take 1024 |> findSteps (70, 70) with
    | Some result -> printfn $"Day 18 - Part 1: %d{result}"
    | None -> failwithf "No path found after 1024 blocks!"

    let firstFailingIndex = binarySearchFirstImpossible (70, 70) input
    let xRes, yRes = input |> Seq.take firstFailingIndex |> Seq.last
    printfn $"Day 18 - Part 2: %d{xRes},%d{yRes} (after %d{firstFailingIndex} steps)"
