module AdventOfCode2024.Day14

open System.Text.RegularExpressions
open AdventOfCode2024.Common

let lineRegex = Regex "p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)"

let parseLine line =
    let m = lineRegex.Match line

    match m.Groups |> Seq.map _.Value |> Seq.tail |> Seq.toList with
    | [ Int64 px; Int64 py; Int64 vx; Int64 vy ] -> ((px, py), (vx, vy))
    | _ -> failwithf $"Invalid line: {line}"

let positiveMod (x: int64) (y: int64) : int64 = (x % y + y) % y

let modBounds (xBound, yBound) (x, y) =
    (positiveMod x xBound, positiveMod y yBound)

let getPositionAtTime bounds (px, py) (vx, vy) t =
    let x = px + (vx * t)
    let y = py + (vy * t)
    modBounds bounds (x, y)

let getSafetyScore (xBound, yBound) positions =
    let xCentre, yCentre = (xBound / 2L, yBound / 2L)

    let topLeftCount =
        positions |> Seq.filter (fun (x, y) -> x < xCentre && y < yCentre) |> Seq.length

    let topRightCount =
        positions |> Seq.filter (fun (x, y) -> x > xCentre && y < yCentre) |> Seq.length

    let bottomLeftCount =
        positions |> Seq.filter (fun (x, y) -> x < xCentre && y > yCentre) |> Seq.length

    let bottomRightCount =
        positions |> Seq.filter (fun (x, y) -> x > xCentre && y > yCentre) |> Seq.length

    topLeftCount * topRightCount * bottomLeftCount * bottomRightCount

let printPositions (xBound, yBound) positions =
    let positionSet = positions |> Set.ofSeq

    for y in 0L .. yBound - 1L do
        for x in 0L .. xBound - 1L do
            if Set.contains (x, y) positionSet then
                printf "#"
            else
                printf "."

        printfn ""


let solve () =
    let robots = readLines "14" |> Seq.map parseLine

    let bounds = (101L, 103L)

    let finalPositions =
        robots |> Seq.map (fun (p, v) -> getPositionAtTime bounds p v 100L)

    let safetyScore = getSafetyScore bounds finalPositions

    printfn $"Day 14 - Part 1: %d{safetyScore}"

    printfn "Day 14 - Part 2:"

    Seq.init 10000 id
    |> Seq.map (fun t -> t, robots |> Seq.map (fun (p, v) -> getPositionAtTime bounds p v t))
    |> Seq.sortBy (snd >> getSafetyScore bounds)
    |> Seq.take 1 // Originally 10, but turns out 1 is high enough!
    |> Seq.iter (fun (t, positions) ->
        printfn $"Time: %d{t}"
        printPositions bounds positions)
