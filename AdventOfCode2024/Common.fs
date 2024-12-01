module AdventOfCode2024.Common

open System
open System.IO

let splitBy (separator: string) (inputString: string) : string list =
    inputString.Split([| separator |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let readFile day =
    File.ReadLines $"Inputs/{day}.txt" |> Seq.filter (fun x -> x.Length > 0)
