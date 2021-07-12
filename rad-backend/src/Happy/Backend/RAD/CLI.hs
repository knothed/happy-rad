module Happy.Backend.RAD.CLI(Flag(..), options, optionsWithoutOutfile, hasRADFlag, parseFlags) where

import Happy.Backend.RAD
import System.Console.GetOpt

-------- CLI flags and options --------

data Flag = 
    OptRAD
  | OptRAD_Outfile String
  | OptRAD_Comments
  | OptRAD_ShowTypes
  | OptRAD_RankNTypes
  | OptRAD_ParserType String
  deriving Eq

options :: [OptDescr Flag]
options = [
    Option "r" ["rad"] (NoArg OptRAD) "generate a recursive ascent-descent parser",
    Option "o" ["outfile"] (ReqArg OptRAD_Outfile "FILE") "write the output to FILE (default: file.hs)",
    Option "" ["comments"] (NoArg OptRAD_Comments) "annotate each function with its items (rad-only)",
    Option "" ["types"] (NoArg OptRAD_ShowTypes) "declare the type of all functions (rad-only)",
    Option "" ["rank-n"] (NoArg OptRAD_RankNTypes) "use when some nonterminals have rank-n-types (n > 1) (rad-only)",
    Option "" ["parsertype"] (ReqArg OptRAD_ParserType "NAME") "use NAME instead of `Parser` as the type for the parser wrapper. Only relevant in conjunction with --types."
  ]
    
-- Same as `options`, but missing the `outfile` option. Useful when using `rad-backend` together with happy's `backend` (which also provides an outfile option).
optionsWithoutOutfile :: [OptDescr Flag]
optionsWithoutOutfile = deleteAt 1 options where
  deleteAt idx xs = lft ++ rgt
    where (lft, (_:rgt)) = splitAt idx xs

-------- [Flag] to RADBackendArgs conversion --------

-- If the OptRAD flag is not set, return Nothing.
parseFlags :: [Flag] -> String -> Maybe RADBackendArgs
parseFlags cli base = if not (hasRADFlag cli) then Nothing else Just RADBackendArgs {
  outFile = getOutputFileName base cli,
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

getOutputFileName :: String -> [Flag] -> String
getOutputFileName base cli = case [ s | (OptRAD_Outfile s) <- cli ] of
  []    -> base ++ ".hs"
  list  -> last list