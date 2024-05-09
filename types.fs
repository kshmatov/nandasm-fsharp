module types

type Symbols = Map<string, uint16>

type Op =
    | Zero = 0b0101010us
    | One = 0b0111111us
    | MinusOne = 0b0111010us
    | DReg = 0b0001100us
    | AReg = 0b0110000us
    | Memory = 0b1110000us
    | NotDReg = 0b0001101us
    | NotAReg = 0b0110001us
    | NotMemory = 0b1110001us
    | MinusDreg = 0b0001111us
    | MinusAReg = 0b0110011us
    | MinusMemory = 0b1110011us
    | IncDReg = 0b0011111us
    | IncAReg = 0b0110111us
    | IncMemory = 0b1110111us
    | DecDreg = 0b0001110us
    | DecAReg = 0b0110010us
    | DecMemory = 0b1110010us
    | DPlusA = 0b0000010us
    | DPlusMem = 0b1000010us
    | DMinusA = 0b0010011us
    | DMinusMem = 0b1010011us
    | AMinuxD = 0b0000111us
    | MemMinuxD = 0b1000111us
    | DAndA = 0b0000000us
    | DAndMem = 0b1000000us
    | DOrA = 0b0010101us
    | DOrMem = 0b1010101us
    | Halt = 0b0011110us

type Dest =
    | None = 0b000us
    | A = 0b100us
    | M = 0b001us
    | D = 0b010us
    | AM = 0b101us
    | AD = 0b110us
    | MD = 0b011us
    | AMD = 0b111us

type Jump =
    | Jgt = 0b001us
    | Jeq = 0b001us
    | Jge = 0b011us
    | Jlt = 0b100us
    | Jne = 0b101us
    | Jle = 0b110us
    | Jmp = 0b111us
    | None = 0b000us

type CInstruction = { Comp: Op; Dest: Dest; Jump: Jump }

type Instruction =
    | CInstruction of CInstruction
    | ALabel of string
    | AAddress of uint16
    | Label of string
