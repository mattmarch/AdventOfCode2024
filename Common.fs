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

let (|Int64|_|) (str: string) =
    match Int64.TryParse(str) with
    | (true, integer) -> Some(integer)
    | _ -> None

let unpack2 (l: 'a seq) =
    match l |> Seq.toList with
    | [ a; b ] -> (a, b)
    | _ -> failwith "Expected a list of length 2"

let splitSeq (predicate: 'a -> bool) (input: 'a seq) : ('a seq * 'a seq) =
    let groups = input |> Seq.groupBy predicate |> Seq.toList

    match groups with
    | [ (true, trues); (false, falses) ] -> (trues, falses)
    | [ (false, falses); (true, trues) ] -> (trues, falses)
    | [ (true, trues) ] -> (trues, [])
    | [ (false, falses) ] -> ([], falses)
    | [] -> ([], [])
    | _ -> failwithf "Impossible output from groupBy in splitSeq"

type Vec2d = int * int

type Vec2dL = int64 * int64

let addVec2d (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let subVec2d (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

let greatestCommonFactor a b =
    let rec gcf a b =
        if a = 0 then b
        elif a < b then gcf a (b - a)
        else gcf (a - b) b

    gcf (abs a) (abs b)


let charToInt (c: char) = int c - int '0'
