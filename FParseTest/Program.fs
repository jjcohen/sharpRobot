module Prog

open FParsec
open Domain
open FParsec.Pipes
open Parsers

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let spt = (many (%" " <|> %"\t"))
let rComment = %% spt -- % "#" -- +. restOfLine false -|> Comment

let rPath =
    let pathPart =
        many1CharsTillApply
            (noneOf " \t\n <>#%{}|\"\\^~[]`")
            (% '/')
            (fun a b -> a + b.ToString())
    %% +. (%"/")
    -- +. many pathPart
    // optional path modifiers! eg. .csv?
    -|> fun x y -> x + (y |> List.fold (+) "")

let rx text parser =
    spaces
    >>. skipStringCI text
    >>. spaces
    >>. skipString ":"
    >>. spaces
    >>. parser
    .>> optional spt
    .>> optional rComment
    .>> newline

let rUserAgent = rx "user-agent" (many1Chars (noneOf ". \t\n <>#%{}|\"\\^~[]`")) |>> UserAgent
let rDisallow = rx "disallow" rPath |>> Path |>> Disallow 
let rAllow = rx "allow" rPath |>> Path |>> Allow
//let rSitemap = rx "sitemap" usableChars // TODO: Gotta fix this with a proper URL parser

let rGroupLine, rGroupLineRef = createParserForwardedToRef<GroupLine, unit>()
let rNonGrouplLine, rNonGroupLineRef = createParserForwardedToRef<NonGroupLine, unit>()
let rLine, rLineRef = createParserForwardedToRef<Line, unit>()

do rGroupLineRef := choice [rDisallow; rAllow]
do rNonGroupLineRef := choice [rComment]
do rLineRef := choice [rGroupLine |>> GroupLine; rNonGrouplLine |>> NonGroupLine]

let rulesGrp = 
    %% +.(many1 rUserAgent)
    -- +.(many1 rGroupLine)
    -|> fun userAgents groupLines ->
             (userAgents |> Seq.ofList,
              groupLines |> Seq.ofList)
              |> RulesGroup

let robots = many1 rulesGrp

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test robots @"User-agent: bob
User-agent: saget
disallow : /chicken/feet/wizard/
disallow : /polkadot/
allow    : /oligarch/
User-agent: animal
disallow : /house/
"





//User-agent : dastardly
//disallow   : /ouch/
//disallow   : /polkadot/ # the magic
//allow      : /polkadot/
//"


//// successes
//printf "Successes"
//test splitLines "line1\nline2\n"
//test rComment "# Super comment !!!!"
//test rComment "###### Super comment !!!!"
//test rComment "      # Super comment !!!!"
//test rUserAgent "User-agent: *"
//test rUserAgent "  User-agent : cheeze-wiz"
//test rUserAgent "User-agent: alphabet #"
//test rUserAgent "User-agent: alphabet #dees is ma wuunderful comment"
//test rGroupLine "disallow : /cheese/it/"
//test rGroupLine "disallow : /cran/forest/ # cranberry forest"
//test rGroupLine "Disallow : / "
//test rGroupLine "allow : /cheese/it/"
//test rGroupLine "Disallow : /gorp/"
//test rGroupLine "allow : /gorp/blorp/"

//// failures
//printf "Failures"
//test rPath "//"
//test rPath "/gorp" 
//test rPath "/gorp/blorp"
//test rPath "/gorp<blorp/"
//test rPath """/gorp"blorp/"""
//test rPath "/gorp     /blorp/"
//test rPath "/gorp  #   /blorp/"



//let robotsParser = 
    //%% spaces
    //-- optional (many (newline >>. rComment))
    //-- +.(many1 (rGroup))
    //-- spaces
    //-- (many1 (rNonGrouplLine))
    //-|> fun x -> x

//let robotsParser = 
    //spaces
    //>>. optional (many (newline >>. rComment))
    //>>. (many1 (rGroup))
    //.>> spaces
    //.>> (many1 (rNonGrouplLine))



//System.Console.ResetColor()


//// <scheme>://<net_loc>/<path>;<params>?<query>#<fragment>
//let rUrl =
    //(pstring "https" <|> pstring "http")
    //>>. pstring ":/"
    //>>. rPath


//      robotstxt = *entries
//      entries = *( ( <1>*startgroupline 
//        *(groupmemberline | nongroupline | comment)
//        | nongroupline
//        | comment) )
//      startgroupline = [LWS] "user-agent" [LWS] ":" [LWS] agentvalue [comment] EOL
//      groupmemberline = [LWS] (
//        pathmemberfield [LWS] ":" [LWS] pathvalue
//        | othermemberfield [LWS] ":" [LWS] textvalue) [comment] EOL
//      nongroupline = [LWS] (
//        urlnongroupfield [LWS] ":" [LWS] urlvalue
//        | othernongroupfield [LWS] ":" [LWS] textvalue) [comment] EOL
//      comment = [LWS] "#" *anychar
//      agentvalue = textvalue
//
//      pathmemberfield = "disallow" | "allow"
//      othermemberfield = ()
//      urlnongroupfield = "sitemap"
//      othernongroupfield = ()
//     
//      pathvalue = "/" path
//      urlvalue = absoluteURI
//      textvalue = *(valuechar | SP)
//      valuechar = <any UTF-8 character except ("#" CTL)>
//      anychar = <any UTF-8 character except CTL>
//      EOL = CR | LF | (CR LF)


// examples 

//let vue = 
    //"""
    ////Sitemap: https://www.myvue.com/sitemap.xml
    ////User-agent: *
    ////Disallow: /data/
    ////Disallow: /partials/
    //"""

// let `empty file` =
    //""""""

// let `allow no robots` = 
    //"""
    //User-agent: *
    //Disallow: /
    //"""

// let `allow all past` = 
    //"""
    //User-agent: *
    //Disallow:
    //"""

// TODO: put actual tab charachters in this to test tabs
// let `only comments will allow robot to scrape all` = 
    //"""
    //# Only 
    //  # comments
    //  # make
    //   # robots.txt
    //      # useless
    //"""

// let `simple real world test` = 
    //"""
    //# robots.txt for http://www.example.com/

    //User-agent: *
    //Disallow: /cyberworld/map/ # This is an infinite virtual URL space

    //# Cybermapper knows where to go.
    //User-agent: cybermapper
    //Disallow: /
    //"""

// let `barbican example robots` = 
    //"""
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
    //Allow: /core/*.css$
    //Allow: /core/*.css?
    //Allow: /core/*.js$
    //Allow: /core/*.js?
    //Allow: /core/*.gif
    //Allow: /core/*.jpg
    //Allow: /core/*.jpeg
    //Allow: /core/*.png
    //Allow: /core/*.svg
    //Allow: /profiles/*.css$
    //Allow: /profiles/*.css?
    //Allow: /profiles/*.js$
    //Allow: /profiles/*.js?
    //Allow: /profiles/*.gif
    //Allow: /profiles/*.jpg
    //Allow: /profiles/*.jpeg
    //Allow: /profiles/*.png
    //Allow: /profiles/*.svg
    //# Directories
    //Disallow: /core/
    //Disallow: /profiles/
    //# Files
    //Disallow: /README.txt
    //Disallow: /web.config
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