module utils

open System

let getFile fn = 
    fn |> IO.File.ReadAllLines
    |> List.ofArray

let storeFile (fn:string) (lines: string list) =
    IO.File.WriteAllLines( fn, lines)