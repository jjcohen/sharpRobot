///AST
module Domain

// Small domain model of the Robots.txt
type URL = URL of string
type Path = Path of string
type UserAgent = UserAgent of string

type NonGroupLine =
| Comment of string
//| Sitemap of URL

type GroupLine =
| Disallow of Path
| Allow of Path

type Line = 
| GroupLine of GroupLine
| NonGroupLine of NonGroupLine

type RulesGroup = RulesGroup of (UserAgent seq) * (GroupLine seq) //* (NonGroupLine seq) option
type RobotsTxt = Root of URL * (NonGroupLine seq) option * RulesGroup seq

//type RulesGroup = UserAgent * (GroupLine Set) * (NonGroupLine Set) option
//type RobotsTxt = Root of URL * (NonGroupLine Set) option * RulesGroup Set