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
    | _ -> failwithf $"unknown operation: {s}"


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
    | _ -> failwith "bad jump condition: {s}"

let parseDest(s: string): Dest = 
    match s with
    | "A" -> Dest.A
    | "D" -> Dest.D
    | "M" -> Dest.M
    | "AM" -> Dest.AM
    | "AD" -> Dest.AD
    | "MD" -> Dest.MD
    | "AMD" -> Dest.AMD
    | "" -> Dest.None
    | _ -> "unknown destination: {s}" |> failwith

let parseCInstruction (s: string): CInstruction =
    let op_jmp = s.Split(";")
    let des_op = op_jmp[0].Split("=")
    let jmp = 
        match op_jmp |> Array.length with
        | 2 -> op_jmp[1] |> parseJmp
        | 1 -> Jump.None
        | _ -> $"wrong syntax: {s}" |> failwith
    
    let dest, exp = 
        match des_op |> Array.length with
        | 2 -> des_op[0] |> parseDest, des_op[1] |> parseComp
        | 1 -> Dest.None, des_op[0] |> parseComp
        | _ -> $"wrong syntax: {s}" |> failwith
    
    {Comp=exp; Dest=dest; Jump=jmp}

let parseAInstruction (s: string): Instruction = 
    try
        s |> uint16 |> AAddress
    with
    | _ ->  s |> ALabel

let parse (s: string):Instruction= 
    match s with
    | x when x.StartsWith("(") && x.EndsWith(")") -> s.Trim([|'(';')'|]) |> Label
    | x when x.StartsWith("@") -> s.Substring(1) |> parseAInstruction
    | _ -> parseCInstruction s  |> CInstruction

let Parse (s: string): Instruction option = 
    if s.StartsWith("//") then
        None
    else
        let cleared = s.Split("//")
        cleared[0].Trim() |> parse |> Some

