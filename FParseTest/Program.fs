module Prog

open AST
open FParsec
open Parsers
open NUnit.Framework
open System.Timers

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

//test robots @"
//User-agent: *
//Disallow: /data/
//Disallow: /partials/
//"

//printfn ""
//printfn ""

//test robots @"# robots.txt for http://www.example.com/

//User-agent: *
//Disallow: /cyberworld/map/ # This is an infinite virtual URL space

//# Cybermapper knows where to go.
//User-agent: cybermapper
//Disallow: /
//"

//printfn ""
//printfn ""


//# robots.txt for http://www.example.com/
//# sitemap: http://www.cheese.gov.uk/sitemap.xml

test robots @"

# This will set then Rules for everyone
User-agent: *
Disallow: /cyberworld/map/ # This is an infinite virtual URL space
Disallow: /chicken/
# MY AMAZING COMMENT

User-agent: cybermapper
User-agent: theDon
Disallow: /
"

//test robots """
//#
//# robots.txt
//#
//# This file is to prevent the crawling and indexing of certain parts
//# of your site by web crawlers and spiders run by sites like Yahoo!
//# and Google. By telling these "robots" where not to go on your site,
//# you save bandwidth and server resources.
//#
//# This file will be ignored unless it is at the root of your host:
//# Used:    http://example.com/robots.txt
//# Ignored: http://example.com/site/robots.txt
//#
//# For more information about the robots.txt standard, see:
//# http://www.robotstxt.org/robotstxt.html 
//User-agent: *
//# CSS, JS, Images
//Allow: /core/
//Allow: /profiles/
//# Directories
//Disallow: /core/
//Disallow: /profiles/
//# Files
//# Disallow: /README.txt
//# Disallow: /web.config
//# Paths (clean URLs)
//Disallow: /admin/
//Disallow: /comment/reply/
//Disallow: /filter/tips/
//Disallow: /node/add/
//Disallow: /search/
//Disallow: /user/register/
//Disallow: /user/password/
//Disallow: /user/login/
//Disallow: /user/logout/
//# Paths (no clean URLs)
//Disallow: /index.php/admin/
//Disallow: /index.php/comment/reply/
//Disallow: /index.php/filter/tips/
//Disallow: /index.php/node/add/
//Disallow: /index.php/search/
//Disallow: /index.php/user/password/
//Disallow: /index.php/user/register/
//Disallow: /index.php/user/login/
//Disallow: /index.php/user/logout/
//"""



//printfn ""
//printfn ""

//test url @"http://www.cheese.gov.uk/donkeys/"
//test url @"www.cheese.gov.uk/"
//test url @"www.cheese.gov.uk"


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