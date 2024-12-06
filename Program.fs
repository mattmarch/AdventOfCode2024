open AdventOfCode2024
open Common

printfn "Advent of Code 2024 solutions"

let solutions =
    [ Day01.solve; Day02.solve; Day03.solve; Day04.solve; Day05.solve; Day06.solve ]

let args = System.Environment.GetCommandLineArgs()[1..] |> Array.toList

match args with
| [] ->
    printfn "Running all solutions"
    solutions |> List.iter (fun solve -> solve ())
| [ Integer i ] ->
    printfn $"Running solution for day {i}"
    solutions.[i - 1] ()
| _ -> failwithf "Invalid arguments, expected a single day number."
