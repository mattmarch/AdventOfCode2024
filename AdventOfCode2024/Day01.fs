module AdventOfCode2024.Day01

open System
open System.IO

let splitBy (separator: string) (inputString: string) : string list =
    inputString.Split([| separator |], StringSplitOptions.None) |> Array.toList

let parseLine line =
    match line |> splitBy "   " |> List.map int with
    | [ a; b ] -> (a, b)
    | _ -> failwith "Invalid input"

let itemSimularityScore list item =
    item * (list |> Seq.filter ((=) item) |> Seq.length)

let solve =
    
    let inputLines =
        File.ReadLines "Inputs/01.txt" |> Seq.filter (fun x -> x.Length > 0)

    let leftList, rightList =
        inputLines |> Seq.map parseLine |> Seq.toList |> List.unzip

    let differencesSum =
        Seq.zip (List.sort leftList) (List.sort rightList)
        |> Seq.sumBy (fun (a, b) -> abs (b - a))

    printfn $"Day 01 - Part 1: %d{differencesSum}"
    
    let simularityScore =
        leftList |> Seq.sumBy (itemSimularityScore rightList)
    
    printfn $"Day 01 - Part 2: %d{simularityScore}"
    
    
    