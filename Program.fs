open System

let handleFile fn = 
    if fn |> IO.Path.Exists |> not then
        printfn "%s not found" fn
        exit 0

    let path = IO.Path.GetDirectoryName fn
    let name = IO.Path.GetFileNameWithoutExtension fn
    let resultFile = IO.Path.Join [|path;name+".hack"|]

    fn 
    |> utils.getFile
    |> translator.Translate
    |> utils.storeFile resultFile


let rec handleFiles (fns: string list) =
    match fns with
    | [] -> printfn "done"
    | x :: xs ->
        handleFile x
        handleFiles xs

[<EntryPoint>]
let main argv = 
    argv |> List.ofArray |> handleFiles
    0
