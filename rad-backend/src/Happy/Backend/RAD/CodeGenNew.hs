module Happy.Backend.RAD.CodeGenNew where
  import Happy.Backend.RAD.Tools (XGrammar(..), NameSet(..), showItem, showProd, lhs, showRecognitionPoint, recognitionPoints, rhsAfterDot)
  import qualified Happy.Backend.RAD.Tools as RADTools
  import Happy.CodeGen.Common.Options
  import Happy.Grammar
  import Happy.Tabular
  import Happy.Tabular.LALR
  import Happy.Backend.RAD.StateGen
  import Control.Monad
  import Data.List
  import Data.Maybe
  import Data.Text (pack, unpack, replace)
  import GHC.Arr ((!), indices)
  import Language.Haskell.TH
  import Language.Haskell.Meta.Parse

  data ParserType = Normal | Monad | MonadLexer deriving (Eq, Show)

  data GenOptions = GenOptions {
    ptype :: ParserType,
    
    wrapperType :: String, -- e.g. "Parser"
    errorTokenType :: String, -- e.g. "ErrorToken"
    
    showTypes :: Bool,
    comments :: Bool,
    
    rank2Types :: Bool, -- when True, all functions (including goto functions) which use or enclose a higher-rank-function are annotated with an explicit type.
    forallMatch :: String, -- the text which determines which types count as rank-2-types.
    
    rulesTupleBased :: Bool, -- actually, this doesn't produce good nor fast code. maybe drop this?

    optimize :: Bool -- inline all rule functions and eta-expand all applications of local "g" functions
  } deriving Show

  genCode :: GenOptions -> XGrammar -> [RADState] -> ActionTable -> GotoTable -> [Int] -> IO String
  genCode opts x states action goto unused_rules = do
    code <- runQ $ genCodeQ opts x states action goto unused_rules
    return $ pprint code

  genCodeQ :: GenOptions -> XGrammar -> [RADState] -> ActionTable -> GotoTable -> [Int] -> Q [Dec]
  genCodeQ opts x states action goto unused_rules = do
    entries <- mapM (genEntryPoint opts x states) (starts g)
    let tail = genTail x
    return $  tail
    where
    g = (RADTools.g x)
    common = (RADTools.common x)

  genTail :: XGrammar -> [Dec]
  genTail x = maybe [] convert (tl x) where
      convert tl = case parseDecs tl of
          Left err -> error err
          Right res -> res

  genEntryPoint :: GenOptions -> XGrammar -> [RADState] -> (String, Int, Int, Bool) -> DecQ
  genEntryPoint opts x@XGrammar { RADTools.g = g, RADTools.common = common_ } states (name, lhs, rhs, isPartial) = do
      let decl = [| $(ruleF prod) ($(parseF (eof_term g)) . const . returnP) Nothing |]
      funD (mkName name) [clause [] (normalB (decl)) []] where
        prod = fromJust $ find matches [0 .. length (productions g) - 1] where
        matches i = matches' (lookupProdNo g i)
        matches' (Production lhs' rhs' _ _) = lhs' == lhs && rhs' == [rhs]      

  parseF :: Int -> ExpQ
  parseF i = varE $ mkName ("parse" ++ show i)

  ruleF :: Int -> ExpQ
  ruleF i = varE $ mkName ("rule" ++ show i)