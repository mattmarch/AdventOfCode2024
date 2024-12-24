module AdventOfCode2024.Day15

open AdventOfCode2024.Common

type Direction =
    | Up
    | Down
    | Left
    | Right

let parseInstruction instruction =
    match instruction with
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | _ -> failwithf $"Unexpected instruction {instruction}"

type MapItem =
    | Wall
    | Crate

type ParsedMapItem =
    | MapItem of MapItem
    | Robot

let parseMap (mapInput: string seq) =
    let parsedItems =
        mapInput
        |> Seq.indexed
        |> Seq.collect (fun (y, line) ->
            line
            |> Seq.indexed
            |> Seq.choose (fun (x, line) ->
                match line with
                | '#' -> Some((x, y), MapItem Wall)
                | 'O' -> Some((x, y), MapItem Crate)
                | '@' -> Some((x, y), Robot)
                | _ -> None))

    let robotPosition = parsedItems |> Seq.find (snd >> (=) Robot) |> fst

    let map =
        parsedItems
        |> Seq.choose (function
            | p, MapItem i -> Some(p, i)
            | _, Robot -> None)
        |> Map.ofSeq

    map, robotPosition

type WarehouseMap = Map<Vec2d, MapItem>

type ParsedInput =
    { WarehouseMap: WarehouseMap
      RobotPosition: Vec2d
      Instructions: Direction list }

let parseInput input =
    let map, instructions = input |> splitBy "\n\n" |> unpack2

    let map, robotPosition = parseMap (map |> splitBy "\n")

    { WarehouseMap = map
      RobotPosition = robotPosition
      Instructions = instructions.Replace("\n", "") |> Seq.map parseInstruction |> Seq.toList }

let vecFromDirection =
    function
    | Up -> (0, -1)
    | Down -> (0, 1)
    | Left -> (-1, 0)
    | Right -> (1, 0)

let move map pos dir =
    let rec getEndCratePosition pos dir =
        let nextPos = addVec2d pos (vecFromDirection dir)

        match Map.tryFind nextPos map with
        | Some Crate -> getEndCratePosition nextPos dir
        | Some Wall -> None
        | None -> Some nextPos

    let nextRobotPosition = addVec2d pos (vecFromDirection dir)

    match getEndCratePosition pos dir with
    | None -> pos, map
    | Some noCratePushedPosition when noCratePushedPosition = nextRobotPosition -> nextRobotPosition, map
    | Some endCratePosition -> nextRobotPosition, map |> Map.remove nextRobotPosition |> Map.add endCratePosition Crate

let rec applyInstructions moveFn mapState steps position =
    match steps with
    | next :: remaining ->
        let nextPosition, nextState = moveFn mapState position next
        applyInstructions moveFn nextState remaining nextPosition
    | [] -> mapState

let gps (x, y) = x + (100 * y)

let getResult mapState =
    mapState |> Map.filter (fun _ v -> v = Crate) |> Map.keys |> Seq.sumBy gps

let expandToBigWarehouse warehouseMap =
    warehouseMap
    |> Map.toSeq
    |> Seq.collect (function
        | (x, y), Wall -> [ (2 * x, y), Wall; (2 * x + 1, y), Wall ]
        | (x, y), Crate -> [ (2 * x, y), Crate ])
    |> Map.ofSeq
//
// let movePart2 map pos dir =
//     let rec getPushedCratePositons currentPushed pos dir =
//         let nextPos = addVec2d pos (vecFromDirection dir)
//
//
//
//         match Map.tryFind nextPos map with
//         | Some Crate -> getPushedCratePositons currentPushed nextPos dir
//         | Some Wall -> None
//         | None -> Some nextPos
//
//     let nextRobotPosition = addVec2d pos (vecFromDirection dir)
//
//     match getEndCratePosition pos dir with
//     | None -> pos, map
//     | Some noCratePushedPosition when noCratePushedPosition = nextRobotPosition -> nextRobotPosition, map
//     | Some endCratePosition -> nextRobotPosition, map |> Map.remove nextRobotPosition |> Map.add endCratePosition Crate

let solve () =
    let input = readAllText "15" |> parseInput

    let result =
        applyInstructions move input.WarehouseMap input.Instructions input.RobotPosition
        |> getResult

    printfn $"Day 15 - Part 1: %d{result}"
