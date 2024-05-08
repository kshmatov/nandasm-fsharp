module parser

open types

let parseComp (s: string): Op =
    match s with
    | "0" -> Op.Zero
    | "1" -> Op.One
    | "-1" -> Op.MinusOne
    | "D" -> Op.DReg
    | "A" -> Op.AReg
    | "M" -> Op.Memory
    | "!D" -> Op.NotDReg
    | "!A" -> Op.NotAReg
    | "!M" -> Op.NotMemory
    | "-D" -> Op.MinusDreg
    | "-A" -> Op.MinusAReg
    | "-M" -> Op.MinusMemory
    | "D+1" -> Op.IncDReg
    | "A+1" -> Op.IncAReg
    | "M+1" -> Op.IncMemory
    | "D-1" -> Op.DecDreg
    | "A-1" -> Op.DecAReg
    | "M-1" -> Op.DecMemory
    | "D+A" -> Op.DPlusA
    | "D+M" -> Op.DPlusMem
    | "D-A" -> Op.DMinusA
    | "D-M" -> Op.DMinusMem
    | "A-D" -> Op.AMinuxD
    | "M-D" -> Op.MemMinuxD
    | "D&A" -> Op.DAndA
    | "D&M" -> Op.DAndMem
    | "D|A" -> Op.DOrA
    | "D|M" -> Op.DOrMem
    | _ -> failwithf "unknown operation: %s" s


let parseJmp(s: string): Jump = 
    match s with
    | "JEQ" -> Jump.Jeq
    | "JNE" -> Jump.Jne
    | "JGT" -> Jump.Jgt
    | "JGE" -> Jump.Jge
    | "JLT" -> Jump.Jlt
    | "JLE" -> Jump.Jle
    | "JMP" -> Jump.Jmp
    | "" -> Jump.None
    | _ -> failwithf "bad jump condition: %s" s

let parseDest(s: string): Dest Set = 
    s.ToCharArray() 
    |> Array.map (fun x ->
        match x with
        | 'A' -> Dest.A
        | 'D' -> Dest.D
        | 'M' -> Dest.M
        | _ -> failwithf "unknown destination: %s" s
    )
    |> Set.ofArray

let parseCInstruction (s: string): CInstruction =
    let op_jmp = s.Split(";")
    let des_op = op_jmp[0].Split("=")
    let jmp = 
        match op_jmp |> Array.length with
        | 2 -> parseJmp op_jmp[1]
        | 1 -> Jump.None
        | _ -> failwithf "wrong syntax: %s" s
    
    let dest, exp = 
        match des_op |> Array.length with
        | 2 -> des_op[0] |> parseDest, des_op[1] |> parseComp
        | 1 -> Set.empty, des_op[0] |> parseComp
        | _ -> failwithf "wrong syntax: %s" s
    
    {Comp=exp; Dest=dest; Jump=jmp}

let parseAInstruction (s: string): Instruction = 
    try
        s |> uint16 |> AAddress
    with
    | _ -> ALabel s

let parse (s: string):Instruction= 
    match s[0] with
    | '(' -> s.Trim([|'(';')'|]) |> Label
    | '@' -> s.Substring(1) |> parseAInstruction
    | _ -> parseCInstruction s  |> CInstruction

let Parse (s: string): Instruction option = 
    if s.StartsWith "//" then
        None
    else
        let cleared = s.Split "//"
        cleared[0] |> parse |> Some

