module Happy.Backend.RAD.CLI(Flag(..), options, parseFlags) where

import Happy.Backend.RAD
import System.Console.GetOpt

-------- CLI flags and options --------

data Flag = 
    OptOutfile String |
    OptComments |
    OptShowTypes |
    OptRankNTypes |
    OptParserType String
  deriving Eq

-- All options, without the characteristic "rad" option.
options :: [OptDescr Flag]
options = [
    Option "o" ["outfile"] (ReqArg OptOutfile "FILE") "write the output to FILE (default: file.hs)",
    Option "c" ["comments"] (NoArg OptComments) "annotate each RAD function with its items",
    Option "t" ["types"] (NoArg OptShowTypes) "declare the type of all RAD functions",
    Option "n" ["rank-n"] (NoArg OptRankNTypes) "declare the explicit rank-n type of all RAD functions",
    Option "" ["parsertype"] (ReqArg OptParserType "NAME") "use NAME instead of `Parser` as type for the RAD parser wrapper"
  ]

-------- [Flag] to RADBackendArgs conversion --------

parseFlags :: [Flag] -> String -> RADBackendArgs
parseFlags cli base = RADBackendArgs {
  outFile = getOutputFileName base cli,
  typeAnnotations = getTypeAnnotations cli,
  showComments = OptComments `elem` cli,
  parserWrapperType = getParserType cli
}

getTypeAnnotations :: [Flag] -> TypeAnnotations
getTypeAnnotations cli
  | elem OptRankNTypes cli = RankN
  | elem OptShowTypes cli = Always
  | otherwise = Never

getParserType :: [Flag] -> Maybe String
getParserType cli = case [ s | (OptParserType s) <- cli ] of
  []    -> Nothing
  list  -> Just $ last list

getOutputFileName :: String -> [Flag] -> String
getOutputFileName base cli = case [ s | (OptOutfile s) <- cli ] of
  []    -> base ++ ".hs"
  list  -> last list