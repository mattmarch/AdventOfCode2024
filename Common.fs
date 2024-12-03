module AdventOfCode2024.Common

open System
open System.IO

let splitBy (separator: string) (inputString: string) : string list =
    inputString.Split([| separator |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let readLines day =
    File.ReadLines $"Inputs/{day}.txt" |> Seq.filter (fun x -> x.Length > 0)

let readAllText day =
    File.ReadAllText $"Inputs/{day}.txt" |> _.Trim()

let (|Integer|_|) (str: string) =
    match Int32.TryParse(str) with
    | (true, integer) -> Some(integer)
    | _ -> None
