module AdventOfCode2024.Day20

open AdventOfCode2024.Common

type Tile =
    | Empty
    | Wall
    | Start
    | End

let parseChar c =
    match c with
    | '.' -> Empty
    | '#' -> Wall
    | 'S' -> Start
    | 'E' -> End
    | _ -> failwithf $"Unexpected character {c}"

let parse lines =
    lines
    |> Seq.indexed
    |> Seq.collect (fun (y, line) -> line |> Seq.mapi (fun x c -> (x, y), parseChar c))
    |> Map.ofSeq

let getNeighbours (x, y) =
    [ (x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y) ]

let getNextPos grid pos =
    getNeighbours pos |> List.filter (fun p -> Map.find p grid <> Wall)

let traverseStandardRoute grid =
    let startPos = grid |> Map.findKey (fun _ t -> t = Start)

    let nextPos =
        match getNextPos grid startPos with
        | [ p ] -> p
        | l -> failwithf $"Unexpected number of next positions from start: {l}"

    let rec traverse visitedSoFar from =
        let lastPos, lastScore = List.head visitedSoFar

        match getNextPos grid from |> List.filter ((<>) lastPos) with
        | [ nextPos ] ->
            match Map.find nextPos grid with
            | End -> (nextPos, lastScore + 2) :: (from, lastScore + 1) :: visitedSoFar
            | Empty -> traverse ((from, lastScore + 1) :: visitedSoFar) nextPos
            | otherwise -> failwithf $"Unexpected tile {otherwise}"
        | l -> failwithf $"Unexpected number of next positions: {l}"

    traverse [ (startPos, 0) ] nextPos |> Map.ofList

let getWalls grid =
    grid |> Map.filter (fun _ t -> t = Wall) |> Map.keys

let findCheatTimeSavings route wallToSkip =
    let timesNextToWall =
        getNeighbours wallToSkip |> List.choose (fun p -> Map.tryFind p route)

    let combinations =
        seq {
            for startT in timesNextToWall do
                for endT in timesNextToWall do
                    if startT < endT then
                        yield startT, endT
        }
    // shortcuts are uniquely identified by the wall being skipped and the end - not the start
    combinations
    |> Seq.groupBy snd
    |> Seq.map (fun (_, group) -> group |> Seq.map (fun (startT, endT) -> endT - startT - 2) |> Seq.max)
    |> Seq.filter ((<) 0)

let manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let find20psCheatTimeSavings route fromPos fromT =
    route
    |> List.filter (fun (p, _) -> manhattanDistance fromPos p <= 20)
    |> List.map (fun (p, t) -> t - fromT - (manhattanDistance fromPos p))
    |> List.filter ((<) 0)

let solve () =
    let grid = readLines "20" |> parse
    let route = traverseStandardRoute grid
    let walls = getWalls grid

    let result =
        walls
        |> Seq.collect (findCheatTimeSavings route)
        |> Seq.filter ((<=) 100)
        |> Seq.length

    printfn $"Day 20 - Part 1: %d{result}"

    let routeList = route |> Map.toList

    let result2 =
        routeList
        |> List.collect (fun (pos, t) -> find20psCheatTimeSavings routeList pos t)
        |> List.filter ((<=) 100)
        |> List.length

    printfn $"Day 20 - Part 2: %d{result2}"
