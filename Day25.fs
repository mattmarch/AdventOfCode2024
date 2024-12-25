module AdventOfCode2024.Day25

open AdventOfCode2024.Common


type KeyOrLock =
    | Key of int list
    | Lock of int list

let parseOtherLines (lines: string list) =
    let valueLines = lines |> List.take 5
    List.init 5 (fun i -> valueLines |> List.sumBy (fun l -> if l[i] = '#' then 1 else 0))

let parseKeyOrLock input =
    match input |> splitBy "\n" with
    | "....." :: otherLines -> Key(parseOtherLines otherLines)
    | "#####" :: otherLines -> Lock(parseOtherLines otherLines)
    | _ -> failwithf $"Unexpected input {input}"

let parseInput = splitBy "\n\n" >> List.map parseKeyOrLock

let checkFits key lock =
    Seq.forall2 (fun k l -> k + l <= 5) key lock

let solve () =
    let locksAndKeys = readAllText "25" |> parseInput

    let locks =
        locksAndKeys
        |> List.choose (function
            | Lock l -> Some l
            | _ -> None)

    let keys =
        locksAndKeys
        |> List.choose (function
            | Key k -> Some k
            | _ -> None)

    let result =
        locks
        |> Seq.sumBy (fun lock -> keys |> Seq.filter (checkFits lock) |> Seq.length)

    printfn $"Day 25 - Part 1: %d{result}"
