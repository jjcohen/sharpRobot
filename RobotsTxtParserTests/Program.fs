module Tests

open AST
open FParsec
open Parsers
open NUnit.Framework

let test parser input expected =
    match run parser input with
    | Success(result, _, _)   -> Assert.AreEqual(expected, result)
    | Failure(errorMsg, _, _) -> Assert.Fail()

let passFail parser input =
    match run parser input with
    | Success(result, _, _)   -> Assert.Pass()
    | Failure(errorMsg, _, _) -> Assert.Fail()

[<Test>]
let ``Extract a single character comment`` =
    test rComment "#C" (Comment "C")

[<Test>]
let ``Extract a zero character comment`` =
    test rComment "#" (Comment "")

[<Test>]
let ``When a comment is parsed the comment character is removed`` =
    test rComment "#Comment" (Comment "Comment")
   
[<Test>]
let ``When a comment is parsed leading spaces and the comment charachter are removed`` =
    test rComment "      # Comment" (Comment " Comment")