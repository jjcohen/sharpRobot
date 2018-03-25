module Parsers

open FParsec
open AST
open FParsec.Pipes

let spt = (many (%" " <|> %"\t"))
let comment = spt >>. %"#" >>. restOfLine false |>> Comment
let blankLine = newline |>> BlankLine

//TODO: optional path modifiers! eg. .csv?
let relativePath =
    %% +. (%"/")
    -- +. many
            (many1CharsTillApply
                (noneOf " \t\n <>#%{}|\"\\^~[]`")
                (% '/')
                (fun a b -> a + b.ToString()))
    -|> List.fold (+)

let possiblePath = relativePath <|> %"/"

let netLocation : Parser<string,unit> =
    sepBy (manyChars (noneOf ". \t\n / <>#%{}|\"\\^~[]`")) %"."
    |>> String.concat "."

//let rParams = %";" .>>. (manyChars (noneOf ". \t\n / <>#%{}|\"\\^~[]`")) |>> fun (x, y) -> x + y

let optionToString (input : string option) = match input with | None -> "" | Some x -> x

//// <scheme>://<net_loc>/<path>;<params>?<query>#<fragment>
let url =
    %% +. (opt (%"https://" <|> %"http://"))
    -- +. netLocation
    -- +. (opt possiblePath) // TODO: path here doesn't need to end with a slash...
    //-- +. (opt (many rParams))
    -|> fun a b c -> (optionToString a) + b + (optionToString c)

let rx text parser =
    spt
    >>. skipStringCI text
    >>. spt
    >>. skipString ":"
    >>. spt
    >>. parser
    .>> spt
    .>> optional comment

let userAgent = rx "user-agent" (many1Chars (noneOf ". \t\n <>#%{}|\"\\^~[]`")) |>> UserAgent
let disallow = rx "disallow" relativePath |>> RelativePath |>> GroupLine.Disallow 
let allow = rx "allow" relativePath |>> RelativePath |>> GroupLine.Allow
let sitemap = rx "sitemap" url |>> URL |>> Sitemap

let groupLine =
    choice [
        disallow
        allow
        comment |>> GroupLine.Comment
        sitemap |>> GroupLine.Sitemap
        blankLine |>> GroupLine.BlankLine ]

let nonGroupLine =
    choice [
        comment |>> NonGroupLine.Comment
        sitemap |>> NonGroupLine.Sitemap
        blankLine |>> NonGroupLine.BlankLine ]

let filterUnnecessary input =
    input
    |> List.filter
        (function
            | GroupLine.Comment _ -> false
            | GroupLine.BlankLine _ -> false
            | _ -> true) 

let filt input =
    match input with
    | Sections.NonGroupLine x ->
        match x with 
        | NonGroupLine.Comment _ -> None
        | NonGroupLine.BlankLine _ -> None
        | _ -> Some input
    | Sections.RulesGroup _ -> Some input
              
let rulesGroup =
    %% +. (sepEndBy1 userAgent newline)
    -- +. many1 groupLine 
    -|> fun userAgents groupLines -> (userAgents, groupLines |> filterUnnecessary) |> Sections.RulesGroup

let robots = many1 (choice [rulesGroup; nonGroupLine |>> Sections.NonGroupLine]) |>> List.choose filt