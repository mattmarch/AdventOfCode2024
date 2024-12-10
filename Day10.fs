module AdventOfCode2024.Day10

open AdventOfCode2024.Common

type Grid = Map<Vec2d, int>

let parseGrid input : Grid =
    input
    |> Seq.mapi (fun y line -> line |> Seq.mapi (fun x c -> (x, y), charToInt c))
    |> Seq.concat
    |> Map.ofSeq

let findTrailheads grid =
    grid |> Map.filter (fun _ h -> h = 0) |> Map.keys

let directions = [ (0, -1); (1, 0); (0, 1); (-1, 0) ]

let findSteps grid targetHeight position =
    directions
    |> List.map (addVec2d position)
    |> List.filter (fun p -> Map.tryFind p grid = Some targetHeight)


let findTrailheadScore (grid: Grid) position =
    let rec walkPaths positions targetHeight =
        let nextPositions =
            positions |> List.collect (findSteps grid targetHeight) |> List.distinct

        match nextPositions, targetHeight with
        | [], _ -> 0
        | endPositions, 9 -> Seq.length endPositions
        | nextPositions, height -> walkPaths nextPositions (height + 1)

    walkPaths [ position ] 1

let findTrailheadRating (grid: Grid) position =
    let rec walkPaths positions targetHeight =
        let nextPositions = positions |> List.collect (findSteps grid targetHeight)

        match nextPositions, targetHeight with
        | [], _ -> 0
        | endPositions, 9 -> Seq.length endPositions
        | nextPositions, height -> walkPaths nextPositions (height + 1)

    walkPaths [ position ] 1

let solve () =
    let grid = readLines "10" |> parseGrid
    let result = grid |> findTrailheads |> Seq.sumBy (findTrailheadScore grid)
    printfn $"Day 10 - Part 1: %d{result}"

    let result2 = grid |> findTrailheads |> Seq.sumBy (findTrailheadRating grid)
    printfn $"Day 10 - Part 2: %d{result2}"
