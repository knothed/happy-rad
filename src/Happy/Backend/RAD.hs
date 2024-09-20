module Happy.Backend.RAD(RADBackendArgs(..), TypeAnnotations(..), runRADBackend) where

import Happy.Grammar
import Happy.Tabular
import Happy.Tabular.LALR
import Happy.Backend.RAD.CodeGen
import Happy.Backend.RAD.StateGen
import Paths_happy_rad
import Data.Maybe

-------- Main entry point (runRADBackend) --------

data RADBackendArgs = RADBackendArgs {
    outFile :: String,
    typeAnnotations :: TypeAnnotations,
    showComments :: Bool,
    parserWrapperType :: Maybe String
}

data TypeAnnotations = Never | Always | RankN deriving Eq -- Rank2 implies Always

runRADBackend :: RADBackendArgs -> Grammar -> Maybe String -> Maybe String -> Pragmas -> ActionTable -> GotoTable -> [Lr1State] -> [Int] -> IO ()
runRADBackend opts g hd tl common action goto items unused_rules =
    let (isMonad, _, parserType, _, _) = monad common

        ptype = case (lexer common, isMonad) of
          (Nothing, False) -> Normal
          (Nothing, True) -> Monad
          (Just _, False) -> error "%lexer without %monad not supported in RAD"
          (Just _, True) -> MonadLexer

        options = GenOptions {
          ptype = ptype,
          wrapperType = fromMaybe (if parserType == "Parser" then "HappyP" else "Parser") (parserWrapperType opts),
          errorTokenType = "ErrorToken",
          showTypes = (typeAnnotations opts) /= Never,
          comments = showComments opts,
          rank2Types = (typeAnnotations opts) == RankN,
          rulesTupleBased = False,
          forallMatch = "forall ",
          optimize = True
        }

        lalrStates = generateLALRStates g action goto items in do

        x <- createXGrammar g hd tl common lalrStates
        radStates <- generateRADStates x lalrStates unused_rules
        genCode options x radStates action goto unused_rules >>=
          if (outFile opts) == "-" then putStr else writeFile (outFile opts)
