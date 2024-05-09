module symbol

open types

let SystemSymbols = Map [
    ("R0", 0us)
    ("R1", 1us)
    ("R2", 2us)
    ("R3", 3us)
    ("R4", 4us)
    ("R5", 5us)
    ("R6", 6us)
    ("R7", 7us)
    ("R8", 8us)
    ("R9", 9us)
    ("R10", 10us)
    ("R11", 11us)
    ("R12", 12us)
    ("R13", 13us)
    ("R14", 14us)
    ("R15", 15us)
    ("SP", 0us)
    ("LCL", 1us)
    ("ARG", 2us)
    ("THIS", 3us)
    ("THAT", 4us)
    ("SCREEN", 0x4000us)
    ("KBD", 0x6000us)
]

let FirstFree = 16us
