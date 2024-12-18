module AdventOfCode2024.Day16

open AdventOfCode2024.Common

let parseInput input =
    let parsedChars =
        input
        |> Seq.indexed
        |> Seq.collect (fun (y, line) -> line |> Seq.mapi (fun x c -> (x, y), c))

    let startPos = parsedChars |> Seq.find (snd >> (=) 'S') |> fst
    let endPos = parsedChars |> Seq.find (snd >> (=) 'E') |> fst

    let walls =
        parsedChars
        |> Seq.choose (function
            | pos, '#' -> Some pos
            | _ -> None)
        |> Set.ofSeq

    startPos, endPos, walls

type Direction =
    | North
    | East
    | South
    | West

let directionToVec2d d =
    match d with
    | North -> (0, -1)
    | East -> (1, 0)
    | South -> (0, 1)
    | West -> (-1, 0)

type ReachableLocation =
    { Position: Vec2d
      Direction: Direction
      Score: int
      Visited: bool }

let traverse walls startPos endPos =
    let rec traverseNext locations =
        let fromLocation =
            locations |> List.filter (fun l -> not l.Visited) |> List.minBy _.Score

        let nextLocations =
            [ North; East; South; West ]
            |> List.map (fun d -> d, d |> directionToVec2d |> addVec2d fromLocation.Position)
            |> List.filter (fun (_d, p) -> not (Set.contains p walls))
            |> List.filter (fun (d, p) -> not (List.exists (fun l -> l.Position = p && l.Direction = d) locations))
            |> List.map (fun (d, p) ->
                { Visited = false
                  Position = p
                  Direction = d
                  Score =
                    if fromLocation.Direction = d then
                        fromLocation.Score + 1
                    else
                        fromLocation.Score + 1001 })

        let updatedLocations =
            locations
            |> List.map (fun l -> if l = fromLocation then { l with Visited = true } else l)
            |> List.append nextLocations

        match nextLocations |> List.tryFind (fun l -> l.Position = endPos) with
        | Some { Score = s } -> s
        | None -> traverseNext updatedLocations

    traverseNext
        [ { Visited = false
            Position = startPos
            Direction = East
            Score = 0 } ]


let solve () =
    let startPos, endPos, walls = readLines "16" |> parseInput
    let result = traverse walls startPos endPos
    printfn $"Day 16 - Part 1: %d{result}"
