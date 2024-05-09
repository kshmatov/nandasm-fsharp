module translator

open System

open utils

open types
open parser
open symbol

type parserState =
    { jumpAddresses: Symbols
      memAddress: Symbols
      curLine: uint16
      free: uint16 }

    member this.AddJumpLabel(l: string) : parserState =
        match this.jumpAddresses.TryFind l with
        | Some _ -> failwith $"label ({l}) is declared already"
        | _ ->
            { this with
                jumpAddresses = this.jumpAddresses.Add(l, this.curLine) }

    member this.GetAddress(l: string) : uint16 * parserState =
        match this.jumpAddresses.TryFind l with
        | Some x -> x, this
        | None ->
            match this.memAddress.TryFind l with
            | Some x -> x, this
            | _ ->
                this.free,
                { this with
                    memAddress = this.memAddress.Add(l, this.free)
                    free = this.free + 1us }

type codeLine = { num: int; instruction: Instruction }

let getAddressFromSymbol (aTable: parserState) (label: string) : (string * parserState) =
    let a, state = label |> aTable.GetAddress
    a |> UnsignedToString, state

let getAddress (a: uint16) = a |> utils.UnsignedToString

let buildOp (i: CInstruction) : string =
    (0b111us <<< 13)
    ||| (i.Comp |> uint16 <<< 6)
    ||| (i.Dest |> uint16 <<< 3)
    ||| (i.Jump |> uint16)
    |> UnsignedToString

let buildInsruction (line: codeLine) (state: parserState) : (string option * parserState) =
    match line.instruction with
    | AAddress address -> address |> getAddress |> Some, state
    | CInstruction i -> i |> buildOp |> Some, state
    | ALabel(label: string) ->
        let a, newState = (state, label) ||> getAddressFromSymbol
        a |> Some, newState
    | x -> $"unexpected instruction on line {line.num}: {x}" |> UnhandledError

let translate (state: parserState) (lines: string list) : string list =
    let numberedLines = lines |> List.indexed

    let code, completeState =
        (state, numberedLines)
        ||> List.mapFold (fun state line ->
            let i, code = line

            try
                match code.Trim() |> Parse with
                | None -> None, state
                | Some op ->
                    match op with
                    | Label l -> None, state.AddJumpLabel l
                    | _ ->
                        { num = i; instruction = op } |> Some,
                        { state with
                            curLine = state.curLine + 1us }
            with err ->
                UnhandledError $"line {i} '{code}' error: {err.Message}")

    let bin, _ =
        (completeState, code)
        ||> List.mapFold (fun state line ->
            match line with
            | None -> None, state
            | Some op -> (op, state) ||> buildInsruction)

    bin |> List.choose (fun x -> x)


let Translate (lines: string list) : string list =
    let state =
        { jumpAddresses = Symbols []
          curLine = 0us
          memAddress = SystemSymbols
          free = FirstFree }

    (state, lines) ||> translate
