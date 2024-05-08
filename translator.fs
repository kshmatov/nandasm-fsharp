module translator

open types
open parser
open symbol

type translatorState = {
    aTable: Symbols
    curLine: uint16
}

let parse (lines: string list) =
    lines
    |> List.mapi (fun x l -> (x, l))
    |> List.choose (fun line ->
        let i, code = line
        try
            code.Trim() |> Parse
        with
        | err -> 
            printfn "line %d '%s' error: %s" i code  err.Message
            exit 0
        )

let getAddressFromSymbol (aTable: Symbols) (label:string): string = 
    match GetAddress aTable label with
    | Some a -> sprintf "0%015B" a
    | _ -> 
        printfn "Cant find label %s in address table" label
        exit 0
let getAddress (a:uint16) = 
    a |> sprintf "0%015B"

let buildOp (i:CInstruction):uint16 =
    let dest = (0, i.Dest) ||> Set.fold (fun x v -> x ||| (v |> int)) |> uint16
    (i.Comp |> uint16 <<< 6) ||| (dest <<< 3) ||| (i.Jump |> uint16)

let buildInsruction (line:Instruction) (state: Symbols) =
    match line with
        | ALabel label -> (state, label) ||> getAddressFromSymbol |> Some
        | AAddress address -> address |> getAddress |> Some
        | CInstruction i -> i |> buildOp |> sprintf "111%013B" |> Some
        | x -> 
            x |> printfn "unexpected instruction %O" 
            exit 0

let translate (aTable: Symbols) (lines: string list): string list = 
    let state = {aTable=aTable;curLine=0b110us}
    let preparedCode: Instruction list = lines |> parse
    // Lets get address table and code without labels
    let parsed, fullState = 
        (state, preparedCode) ||> List.mapFold (fun  state line ->
            match line with
            | Label name -> 
                // Label removed and its line nuber will be the address of Jumps
                None, {aTable=(StoreLabelAddress state.aTable name state.curLine); curLine=state.curLine}
                // All is ok, go on
            | _ -> line |> Some, {state with curLine=state.curLine+1us}
        )
    
    parsed |> List.choose (fun x ->
        match x with
        | None -> None
        | Some line -> (line, fullState.aTable) ||> buildInsruction
            
    )


let Translate (lines: string list): string list =
    let aTable = GetBaseTable()
    lines |> translate aTable
