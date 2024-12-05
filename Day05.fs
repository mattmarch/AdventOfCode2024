module AdventOfCode2024.Day05

open Common

type OrderRule = { Before: int; After: int }

let parseRule ruleLine =
    match ruleLine |> splitBy "|" with
    | [ Integer before; Integer after ] -> { Before = before; After = after }
    | _ -> failwithf $"Invalid rule line {ruleLine}"

let parseInput input =
    let rulesLines, updatePagesLines =
        input |> splitBy "\n\n" |> Seq.map (splitBy "\n") |> unpack2

    let rules = rulesLines |> Seq.map parseRule

    let updatePages =
        updatePagesLines |> Seq.map (fun line -> line |> splitBy "," |> List.map int)

    rules, updatePages

let findPagesBeforePage rules page =
    rules |> Seq.filter (fun rule -> rule.After = page) |> Seq.map _.Before

let checkUpdatePages rules updatePages =
    let rec checkRemaining remainingPages =
        match remainingPages with
        | [] -> true
        | page :: rest ->
            let disallowedPages = findPagesBeforePage rules page

            let isValid =
                disallowedPages |> Seq.exists (fun p -> rest |> List.contains p) |> not

            isValid && checkRemaining rest

    checkRemaining updatePages

let middlePageNum pages =
    let pageIndex = Seq.length pages / 2
    pages |> Seq.item pageIndex

let sortList rules pages =
    let rec addPage sortedPages nextPage others =
        let pagesMustPutBefore = findPagesBeforePage rules nextPage

        let lastPageMustBeAfter =
            sortedPages
            |> Seq.tryFindIndexBack (fun p -> pagesMustPutBefore |> Seq.contains p)

        let insertIndex =
            match lastPageMustBeAfter with
            | Some i -> i + 1
            | None -> 0

        let updatedSortedPages = sortedPages |> List.insertAt insertIndex nextPage

        match others with
        | [] -> updatedSortedPages
        | next :: remaining -> addPage updatedSortedPages next remaining

    match pages with
    | firstPage :: others -> addPage [] firstPage others
    | [] -> []

let solve () =
    let rules, updatePages = readAllText "05" |> parseInput
    let validUpdates, invalidUpdates = updatePages |> splitSeq (checkUpdatePages rules)
    let result = validUpdates |> Seq.sumBy middlePageNum
    printfn $"Day 05 - Part 1: %d{result}"

    let result2 = invalidUpdates |> Seq.map (sortList rules) |> Seq.sumBy middlePageNum
    printfn $"Day 05 - Part 2: %d{result2}"
