module Prog

open AST
open FParsec
open Parsers
open NUnit.Framework

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test robots @"
User-agent: *
Disallow: /data/
Disallow: /partials/
"

printfn ""
printfn ""

test robots @"# robots.txt for http://www.example.com/

User-agent: *
Disallow: /cyberworld/map/ # This is an infinite virtual URL space

# Cybermapper knows where to go.
User-agent: cybermapper
Disallow: /
"

printfn ""
printfn ""

test (many robots) @"# robots.txt for http://www.example.com/

User-agent: *
Disallow: /cyberworld/map/ # This is an infinite virtual URL space
sitemap: http://www.cheese.gov.uk/sitemap.xml

# This is an infinite virtual URL space
User-agent: cybermapper
Disallow: /
"

printfn ""
printfn ""

test url @"http://www.cheese.gov.uk/donkeys/"
test url @"www.cheese.gov.uk/"
test url @"www.cheese.gov.uk"


//let test parser input expected =
//    match run parser input with
//    | Success(result, _, _)   -> Assert.AreEqual(expected, result)
//    | Failure(errorMsg, _, _) -> Assert.Fail()

//let passFail parser input =
//    match run parser input with
//    | Success(result, _, _)   -> Assert.Pass()
//    | Failure(errorMsg, _, _) -> Assert.Fail(errorMsg)

//[<Test>]
//let ``Extract a single character comment`` =
//    test comment "#C" (Comment "C")

//[<Test>]
//let ``Extract a zero character comment`` =
//    test comment "#" (Comment "")

//[<Test>]
//let ``When a comment is parsed the comment character is removed`` =
//    test comment "#Comment" (Comment "Comment")
   
//[<Test>]
//let ``When a comment is parsed leading spaces and the comment charachter are removed`` =
//    test comment "      # Comment" (Comment " Comment")