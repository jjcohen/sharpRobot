module AST

type URL = URL of string
type RelativePath = RelativePath of string
type UserAgent = UserAgent of string

type NonGroupLine =
| BlankLine of char
| Comment of string
| Sitemap of URL

type GroupLine =
| Disallow of RelativePath
| Allow of RelativePath

type Line =
| LineNonGroupLine of NonGroupLine
| LineGroupLine of GroupLine

type RulesGroup = RulesGroup of (UserAgent seq) * (GroupLine seq)

type Sections = 
| SectionsRulesGroup of RulesGroup
| SectionsNonGroupLine of NonGroupLine

type RobotsTxt = RobotsTxt of Sections seq