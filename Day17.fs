module AdventOfCode2024.Day17

open AdventOfCode2024.Common

let parseRegister = splitBy ": " >> unpack2 >> snd >> int

let parseProgram =
    splitBy ": " >> unpack2 >> snd >> splitBy "," >> List.map int >> Array.ofList

type MachineState =
    { A: int
      B: int
      C: int
      Instruction: int
      Output: int list }

let parseInput input =
    let registers, program = input |> splitBy "\n\n" |> unpack2
    let a, b, c = registers |> splitBy "\n" |> Seq.map parseRegister |> unpack3

    { A = a
      B = b
      C = c
      Instruction = 0
      Output = [] },
    program |> parseProgram

let combo state operand =
    match operand with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 2
    | 3 -> 3
    | 4 -> state.A
    | 5 -> state.B
    | 6 -> state.C
    | v -> failwithf $"Unexpected combo operand {v}"

let dvInstruction state operand =
    let numerator = state.A
    let denominator = pown 2 (combo state operand)
    numerator / denominator

let processInstruction state instruction operand =
    match instruction with
    | 0 ->
        { state with
            A = dvInstruction state operand
            Instruction = state.Instruction + 2 }
    | 1 ->
        { state with
            B = state.B ^^^ operand
            Instruction = state.Instruction + 2 }
    | 2 ->
        { state with
            B = (combo state operand) % 8
            Instruction = state.Instruction + 2 }
    | 3 ->
        { state with
            Instruction = if state.A = 0 then state.Instruction + 2 else operand }
    | 4 ->
        { state with
            B = state.B ^^^ state.C
            Instruction = state.Instruction + 2 }
    | 5 ->
        { state with
            Output = (combo state operand) % 8 :: state.Output
            Instruction = state.Instruction + 2 }
    | 6 ->
        { state with
            B = dvInstruction state operand
            Instruction = state.Instruction + 2 }
    | 7 ->
        { state with
            C = dvInstruction state operand
            Instruction = state.Instruction + 2 }
    | _ -> failwithf $"Unexpected instruction {instruction}"

let applyInstructions instructions state =
    let rec applyInstructions' state =
        let instruction = instructions |> Array.tryItem state.Instruction
        let operand = instructions |> Array.tryItem (state.Instruction + 1)

        match instruction, operand with
        | Some instruction, Some operand -> applyInstructions' (processInstruction state instruction operand)
        | _ -> state

    applyInstructions' state

let outputToString state =
    state.Output |> List.rev |> List.map string |> String.concat ","

let solve () =
    let initialState, program = readAllText "17" |> parseInput

    let finalState = applyInstructions program initialState

    let result = finalState |> outputToString

    printfn $"Day 17 - Part 1: %s{result}"
