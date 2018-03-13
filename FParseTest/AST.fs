/// This the the abstract syntax tree for the robots.txt file 
module AST

type URL = URL of string
type RelativePath = RelativePath of string
type UserAgent = UserAgent of string

type NonGroupLine =
| Comment of string
| Sitemap of URL

type GroupLine =
| Disallow of RelativePath
| Allow of RelativePath

type Line = 
| GroupLine of GroupLine
| NonGroupLine of NonGroupLine

type RulesGroup = RulesGroup of (UserAgent seq) * (GroupLine seq) //* (NonGroupLine seq) option
type RobotsTxt = Root of URL * (NonGroupLine seq) option * RulesGroup seq