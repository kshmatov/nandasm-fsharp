module utils

open System

let GetFile fn = 
    fn |> IO.File.ReadAllLines
    |> List.ofArray

let StoreFile (fn:string) (lines: string list) =
    IO.File.WriteAllLines( fn, lines)

let UnhandledError (msg: string) =
    Console.WriteLine(msg)
    exit 0

let UnsignedToString (v:uint16) =
    Convert.ToString(v |> int, 2).PadLeft(16, '0')