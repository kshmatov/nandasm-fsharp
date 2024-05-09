open System

let testData = "   @10
   D=A
   @INFINITE_LOOP
   D;JLE 
   @counter
   M=D
   @SCREEN
   D=A
   @address
   M=D
(LOOP)
   @address
   A=M
   M=-1
   @address
   D=M
   @32
   D=D+A
   @address
   M=D
   @counter
   MD=M-1
   @LOOP
   D;JGT
(INFINITE_LOOP)
   @INFINITE_LOOP
   0;JMP"

let testResult = [
    "0000000000001010";
    "1110110000010000";
    "0000000000010111";
    "1110001100000110";
    "0000000000010000";
    "1110001100001000";
    "0100000000000000";
    "1110110000010000";
    "0000000000010001";
    "1110001100001000";
    "0000000000010001";
    "1111110000100000";
    "1110111010001000";
    "0000000000010001";
    "1111110000010000";
    "0000000000100000";
    "1110000010010000";
    "0000000000010001";
    "1110001100001000";
    "0000000000010000";
    "1111110010011000";
    "0000000000001010";
    "1110001100000001";
    "0000000000010111";
    "1110101010000111";
]

let handleFile fn = 
    if fn |> IO.Path.Exists |> not then
        printfn "%s not found" fn
        exit 0

    let path = IO.Path.GetDirectoryName fn
    let name = IO.Path.GetFileNameWithoutExtension fn
    let resultFile = IO.Path.Join [|path;name+".hack"|]

    fn 
    |> utils.GetFile
    |> translator.Translate
    |> utils.StoreFile resultFile

let runTestSample() =
    let code = 
        testData.Split("\n")
        |> List.ofArray
        |> translator.Translate

    let res =  (code, testResult) ||> List.mapi2 (fun i x y ->
            if x <> y then
                printfn $"line {i} expected '{y}' got '{x}'"
                false
            else
                printf $"line {i} ok"
                true
    )
    if res |> List.contains false then
        printfn "Code has errors."
    else
        printfn "Possibly it works. Need more tests."

let rec handleFiles (fns: string list) =
    match fns with
    | [] -> printfn "done"
    | x :: xs ->
        handleFile x
        handleFiles xs

[<EntryPoint>]
let main argv = 
    if argv |> Array.length > 0 then
        argv |> List.ofArray |> handleFiles
    else
        runTestSample()
    0
