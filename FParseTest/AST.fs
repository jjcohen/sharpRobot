module AST

type URL = URL of string
type RelativePath = RelativePath of string
type UserAgent = UserAgent of string
type BlankLine = BlankLine of char
type Comment = Comment of string
type Sitemap = Sitemap of URL

[<RequireQualifiedAccess>]
type NonGroupLine =
| BlankLine of BlankLine
| Comment of Comment
| Sitemap of Sitemap

[<RequireQualifiedAccess>]
type GroupLine =
| Disallow of RelativePath
| Allow of RelativePath
| Sitemap of Sitemap
| Comment of Comment
| BlankLine of BlankLine

[<RequireQualifiedAccess>]
type Sections = 
| RulesGroup of UserAgent list * GroupLine list
| NonGroupLine of NonGroupLine

type RobotsTxt = Sections list