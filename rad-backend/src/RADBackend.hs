module RADBackend(RADBackendArgs(..), TypeAnnotations(..), runRADBackend) where

import Middleend
import Grammar
import Tables
import RADCodeGen
import RADStateGen
import Paths_rad_backend
import Data.Maybe

-------- Main entry point (runRADBackend) --------

data RADBackendArgs = RADBackendArgs {
    outFile :: String,
    typeAnnotations :: TypeAnnotations,
    showComments :: Bool
}

data TypeAnnotations = Never | Always | RankN deriving Eq -- Rank2 implies Always

runRADBackend :: RADBackendArgs -> Grammar -> ActionTable -> GotoTable -> [Lr1State] -> [Int] -> IO ()
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