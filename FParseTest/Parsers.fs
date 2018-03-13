module Parsers

open FParsec
open AST
open FParsec.Pipes

let spt = (many (%" " <|> %"\t"))
let comment = spt >>. %"#" >>. restOfLine false |>> Comment

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

let rParams = %";" .>>. (manyChars (noneOf ". \t\n / <>#%{}|\"\\^~[]`")) |>> fun (x, y) -> x + y

let optionToString (input : string option) = match input with | None -> "" | Some x -> x

//// <scheme>://<net_loc>/<path>;<params>?<query>#<fragment>
let url =
    %% +. (opt (%"https://" <|> %"http://"))
    -- +. netLocation
    -- +. (opt possiblePath) // TODO: path here doesn't need to end with a slash...
    -- +. (opt (many rParams))
    -|> fun a b c d -> (optionToString a) + b + (optionToString c)

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
let disallow = rx "disallow" relativePath |>> RelativePath |>> Disallow 
let allow = rx "allow" relativePath |>> RelativePath |>> Allow
let sitemap = rx "sitemap" url |>> URL |>> Sitemap

let groupLine, groupLineRef = createParserForwardedToRef<GroupLine, unit>()
let nonGroupLine, nonGroupLineRef = createParserForwardedToRef<NonGroupLine, unit>()

do groupLineRef := choice [disallow; allow]
do nonGroupLineRef := choice [comment; sitemap]

//let groupLine = choice [disallow; allow]
//let nonGroupLine = choice [comment; sitemap]

let rulesGroup =
    %% +.(sepEndBy1 userAgent newline)
    -- (optional comment)
    -- +.(sepEndBy1 groupLine newline)
    -- (optional comment)
    -- (optional newline)
    -|> fun userAgents groupLines ->
             (userAgents |> Seq.ofList,
              groupLines |> Seq.ofList)
              |> RulesGroup

let robots =
    (optional comment)
    >>. spaces
    >>. (optional comment)
    >>. (many1 rulesGroup)
    .>> (optional comment)
    .>> spaces

//let robots =
    //%% +.(opt nonGroupLine)
    //-- spaces
    //-- +.(many1 rulesGroup)
    //-- spaces
    //-- +.(opt nonGroupLine)
    //-- spaces
    //-|> fun 