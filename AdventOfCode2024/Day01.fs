module AdventOfCode2024.Day01

open AdventOfCode2024.Common

let parseLine line =
    match line |> splitBy " " with
    | [ Integer a; Integer b ] -> (a, b)
    | l -> failwith $"Invalid input, found ${List.length l} items on line, expected 2"

let itemSimularityScore list item =
    item * (list |> Seq.filter ((=) item) |> Seq.length)

let solve () =
    let inputLines = readLines "01"

    let leftList, rightList =
        inputLines |> Seq.map parseLine |> Seq.toList |> List.unzip

    let differencesSum =
        Seq.zip (List.sort leftList) (List.sort rightList)
        |> Seq.sumBy (fun (a, b) -> abs (b - a))

    printfn $"Day 01 - Part 1: %d{differencesSum}"

    let simularityScore = leftList |> Seq.sumBy (itemSimularityScore rightList)

    printfn $"Day 01 - Part 2: %d{simularityScore}"
