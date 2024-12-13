module AdventOfCode2024.Day12

open AdventOfCode2024.Common

let parseGrid (input: string seq) =
    input
    |> Seq.mapi (fun y line -> line |> Seq.mapi (fun x c -> (x, y), c))
    |> Seq.concat
    |> Seq.groupBy snd
    |> Seq.map (fun (_, group) -> group |> Seq.map fst |> Set.ofSeq)


let getExistingAdjacentPositions positionsInRegion (x, y) =
    [ (x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y) ]
    |> List.filter (fun p -> Set.contains p positionsInRegion)

type Region = { squares: Set<Vec2d>; perimeter: int }

let rec findRegions (positions: Set<Vec2d>) : Region list =
    let rec traverseRegion (region: Region) from =
        if Set.contains from region.squares then
            region
        else
            let adjacentPositions = getExistingAdjacentPositions positions from
            let gainedPerimeter = 4 - (List.length adjacentPositions)

            adjacentPositions
            |> Seq.fold
                traverseRegion
                { perimeter = region.perimeter + gainedPerimeter
                  squares = Set.add from region.squares }

    let startPosition = Set.minElement positions
    let region = traverseRegion { perimeter = 0; squares = Set.empty } startPosition
    let remainingPositions = positions - region.squares

    if Set.isEmpty remainingPositions then
        [ region ]
    else
        region :: findRegions (positions - region.squares)

let fencePrice region =
    region.perimeter * (Set.count region.squares)

let rotate90 (x, y) = (-y, x)

let cornerDiffs =
    [ (-1, 0); (-1, 1); (0, 1) ]
    |> Seq.unfold (fun corners -> Some(corners, corners |> List.map rotate90))
    |> Seq.take 4
    |> Seq.toList

let isCorner allSquares pos corner =
    let cornerMatches =
        corner |> List.map (fun c -> allSquares |> Set.contains (addVec2d pos c))

    match cornerMatches with
    | [ true; false; true ] -> true // Inner corner
    | [ false; _; false ] -> true // Outer corner
    | [ _; _; _ ] -> false // Not a corner
    | _ -> failwithf $"Invalid cornerMatches {cornerMatches}"

let countCorners allSquares squarePos =
    cornerDiffs |> List.filter (isCorner allSquares squarePos) |> List.length

let countAllCorners squares =
    squares |> Set.toSeq |> Seq.sumBy (countCorners squares)

let fencePrice2 region =
    (countAllCorners region.squares) * (Set.count region.squares)

let solve () =
    let positionsPerPlant = readLines "12" |> parseGrid
    let allRegions = positionsPerPlant |> Seq.collect findRegions
    let result = allRegions |> Seq.sumBy fencePrice
    printfn $"Day 12 - Part 1: %d{result}"

    let result2 = allRegions |> Seq.sumBy fencePrice2
    printfn $"Day 12 - Part 2: %d{result2}"
