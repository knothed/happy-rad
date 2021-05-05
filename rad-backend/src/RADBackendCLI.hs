module RADBackendCLI where

import Frontend
import Middleend
import Grammar
import RADCodeGen
import RADStateGen
import Data.Maybe
import System.Console.GetOpt

-- CLIFlag extension:

data RADBackendFlag = 
    OptRAD
  | OptRAD_Comments
  | OptRAD_ShowTypes
  | OptRAD_RankNTypes
  deriving Eq

isRAD :: [RADBackendFlag] -> Bool
isRAD = elem OptRAD

argInfoRad :: [OptDescr RADBackendFlag]
argInfoRad = [
  Option ['r'] ["rad"] (NoArg OptRAD)
    "generate a recursive ascent-descent parser",
  Option [] ["comments"] (NoArg OptRAD_Comments)
    "annotate each function with its items (rad-only)",
  Option [] ["types"] (NoArg OptRAD_ShowTypes)
    "declare the type of all functions (rad-only)",
  Option ['n'] ["rank-n"] (NoArg OptRAD_RankNTypes)
    "use when some nonterminals have rank-n-types (n > 1) (rad-only)"
  ]

-- RADBackendOpts

data RADBackendOpts = RADBackendOpts {
  outFile :: String,
  typeAnnotations :: TypeAnnotations,
  showComments :: Bool
}

data TypeAnnotations = Never | Always | RankN deriving Eq -- Rank2 implies Always

radFlagsToOpts :: String -> [RADBackendFlag] -> RADBackendOpts
radFlagsToOpts outFile flags = RADBackendOpts {
  outFile = outFile,
  typeAnnotations = if elem OptRAD_RankNTypes flags then RankN else if elem OptRAD_ShowTypes flags then Always else Never,
  showComments = elem OptRAD_Comments flags
}

-- Main function

runRADBackend :: RADBackendOpts -> Grammar -> ActionTable -> GotoTable -> [Lr1State] -> [Int] -> IO ()
runRADBackend opts g action goto items unused_rules =
    let (isMonad, _, parserType, _, _) = monad g
    
        ptype = case (Grammar.lexer g, isMonad) of
          (Nothing, False) -> Normal
          (Nothing, True) -> Monad
          (Just _, False) -> error "%lexer without %monad not supported in RAD"
          (Just _, True) -> MonadLexer
         
        options = GenOptions {
          ptype = ptype,
          wrapperType = if parserType == "Parser" then "HappyP" else "Parser",
          errorTokenType = "ErrorToken",
          header = fromMaybe "" (hd g),
          footer = fromMaybe "" (tl g),
          showTypes = (typeAnnotations opts) /= Never,
          comments = showComments opts,
          rank2Types = (typeAnnotations opts) == RankN,
          rulesTupleBased = False,
          forallMatch = "forall ",
          optimize = True
        }
    
        lalrStates = generateLALRStates g action goto items in do

        x <- createXGrammar g lalrStates
        radStates <- generateRADStates x lalrStates unused_rules
        genCode options x radStates action goto >>=
          if (outFile opts) == "-" then putStr else writeFile (outFile opts)