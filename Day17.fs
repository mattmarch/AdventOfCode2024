module AdventOfCode2024.Day17

open AdventOfCode2024.Common

let parseRegister = splitBy ": " >> unpack2 >> snd >> int64

let parseProgram =
    splitBy ": " >> unpack2 >> snd >> splitBy "," >> List.map int >> Array.ofList

type MachineState =
    { A: int64
      B: int64
      C: int64
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
    | 0L -> 0L
    | 1L -> 1L
    | 2L -> 2L
    | 3L -> 3L
    | 4L -> state.A
    | 5L -> state.B
    | 6L -> state.C
    | v -> failwithf $"Unexpected combo operand {v}"

let dvInstruction state operand =
    let numerator = state.A
    let denominator = pown 2L (combo state operand |> int)
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
            B = (combo state operand) % 8L
            Instruction = state.Instruction + 2 }
    | 3 ->
        { state with
            Instruction = if state.A = 0L then state.Instruction + 2 else int operand }
    | 4 ->
        { state with
            B = state.B ^^^ state.C
            Instruction = state.Instruction + 2 }
    | 5 ->
        { state with
            Output = int ((combo state operand) % 8L) :: state.Output
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

let applyInstructionsProducesInstructions instructions state =
    let reversedInstructions = instructions |> Array.rev |> Array.toList
    let expectedOutputLength = List.length reversedInstructions

    let rec applyInstructions' state =
        let instruction = instructions |> Array.tryItem state.Instruction
        let operand = instructions |> Array.tryItem (state.Instruction + 1)

        match instruction, operand with
        | Some instruction, Some operand ->
            let updatedState = processInstruction state instruction operand

            if instruction = 5 then
                let outputLength = List.length updatedState.Output

                if (reversedInstructions |> List.skip (expectedOutputLength - outputLength)) = updatedState.Output then
                    applyInstructions' updatedState
                else
                    // Halt early if output is not matching
                    updatedState

            else
                applyInstructions' updatedState
        | _ -> state

    (applyInstructions' state |> _.Output) = reversedInstructions

let rec findValueOfAProducesInstructions instructions start limit defaultState =
    if applyInstructionsProducesInstructions instructions { defaultState with A = start } then
        Some start
    else if start = limit then
        None
    else
        findValueOfAProducesInstructions instructions (start + 1L) limit defaultState

let solve () =
    let initialState, program = readAllText "17" |> parseInput

    let finalState = applyInstructions program initialState

    let result = finalState |> outputToString

    printfn $"Day 17 - Part 1: %s{result}"

    match findValueOfAProducesInstructions program 0L 100000000L initialState with
    | Some res -> printfn $"Day 17 - Part 2: %d{res}"
    | None -> printfn $"Day 17 - Part 2: No result found"
