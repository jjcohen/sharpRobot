module Prog

open FParsec
open Parsers

let rawFile = System.IO.File.ReadAllText("Test.txt")

let add =
    pipe3
        pint32
        (pstring " + ")
        pint32
        (fun firstNumber _ignored secondNumber -> 
                firstNumber + secondNumber)

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test add "19 + 23"
//test robots rawFile