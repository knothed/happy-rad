module Happy.Backend.RAD.CLI(Flag(..), options, hasRADFlag, parseFlags) where

import Happy.Backend.RAD
import System.Console.GetOpt

-------- CLI flags and options --------

data Flag = 
    OptRAD
  | OptRAD_Comments
  | OptRAD_ShowTypes
  | OptRAD_RankNTypes
  | OptRAD_ParserType String
  deriving Eq

options :: [OptDescr Flag]
options = [
    -- Outfile option (o) is borrowed from baseline happy-backend – listing it here would cause an option clash in getOpt
    Option "r" ["rad"] (NoArg OptRAD) "generate a recursive ascent-descent parser",
    Option "" ["comments"] (NoArg OptRAD_Comments) "annotate each function with its items (rad-only)",
    Option "" ["types"] (NoArg OptRAD_ShowTypes) "declare the type of all functions (rad-only)",
    Option "" ["rank-n"] (NoArg OptRAD_RankNTypes) "use when some nonterminals have rank-n-types (n > 1) (rad-only)",
    Option "" ["parsertype"] (ReqArg OptRAD_ParserType "NAME") "use NAME instead of `Parser` as the type for the parser wrapper. Only relevant in conjunction with --types."
  ]

-------- [Flag] to RADBackendArgs conversion --------

-- If the OptRAD flag is not set, return Nothing.
parseFlags :: [Flag] -> String -> Maybe RADBackendArgs
parseFlags cli outFile = if not (hasRADFlag cli) then Nothing else Just RADBackendArgs {
  outFile = outFile,
  typeAnnotations = getTypeAnnotations cli,
  showComments = getComments cli,
  parserWrapperType = getParserType cli
}

hasRADFlag :: [Flag] -> Bool
hasRADFlag = elem OptRAD

getTypeAnnotations :: [Flag] -> TypeAnnotations
getTypeAnnotations cli
  | elem OptRAD_RankNTypes cli = RankN
  | elem OptRAD_ShowTypes cli = Always
  | otherwise = Never

getComments :: [Flag] -> Bool
getComments = elem OptRAD_Comments

getParserType :: [Flag] -> Maybe String
getParserType cli = case [ s | (OptRAD_ParserType s) <- cli ] of
  []    -> Nothing
  list  -> Just $ last list