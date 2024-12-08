module AdventOfCode2024.Day08

open AdventOfCode2024.Common

let parseRow y row =
    row
    |> Seq.indexed
    |> Seq.choose (function
        | _, '.' -> None
        | x, c -> Some((x, y), c))

let parseGrid input =
    input |> Seq.mapi parseRow |> Seq.concat

type AntinodeFn = Vec2d -> Vec2d -> Vec2d list

let getAntinodePositions a1 a2 =
    let diff = subVec2d a1 a2
    let an1 = addVec2d a1 diff
    let an2 = subVec2d a2 diff
    [ an1; an2 ]

let getAntinodesOfGroup (antinodeFn: AntinodeFn) antennae =
    let indexedAntennae = Seq.indexed antennae

    let allPairs =
        indexedAntennae
        |> Seq.allPairs indexedAntennae
        |> Seq.filter (fun ((i1, _), (i2, _)) -> i1 < i2)
        |> Seq.map (fun ((_, a1), (_, a2)) -> (a1, a2))

    allPairs |> Seq.map (fun (a1, a2) -> antinodeFn a1 a2) |> Seq.concat

let getAllAntinodes (antinodeFn: AntinodeFn) (antennae: (Vec2d * char) seq) =
    antennae
    |> Seq.groupBy snd
    |> Seq.map (fun (_, group) -> group |> Seq.map fst |> getAntinodesOfGroup antinodeFn)
    |> Seq.concat

let withinBounds (xLim, yLim) (x, y) =
    x >= 0 && x < xLim && y >= 0 && y < yLim

let part2Antinodes limits a1 a2 =
    let dx, dy = subVec2d a1 a2
    let intervalDivisor = greatestCommonFactor dx dy
    let interval = (dx / intervalDivisor, dy / intervalDivisor)

    let forwardsAntinodes =
        List.unfold
            (fun p ->
                if withinBounds limits p then
                    Some(p, addVec2d p interval)
                else
                    None)
            a1

    let backwardsAntinodes =
        List.unfold
            (fun p ->
                if withinBounds limits p then
                    Some(p, subVec2d p interval)
                else
                    None)
            a1

    List.append forwardsAntinodes backwardsAntinodes


let solve () =
    let input = readLines "08"
    let antennae = input |> parseGrid
    let maxY = input |> Seq.length
    let maxX = input |> Seq.head |> String.length
    let antinodes = getAllAntinodes getAntinodePositions antennae

    let result =
        antinodes
        |> Seq.filter (withinBounds (maxX, maxY))
        |> Seq.distinct
        |> Seq.length

    printfn $"Day 08 - Part 1: %d{result}"

    let part2Antinodes = getAllAntinodes (part2Antinodes (maxX, maxY)) antennae
    let result2 = part2Antinodes |> Seq.distinct |> Seq.length
    printfn $"Day 08 - Part 2: %d{result2}"
