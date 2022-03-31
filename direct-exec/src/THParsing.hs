{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}

module THParsing where

import Happy.Frontend
import Happy.Frontend.AbsSyn
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Quote

y :: QuasiQuoter
y = QuasiQuoter {
    quoteExp  = compile
  , quotePat  = error "patterns"
  , quoteType = error "types"
  , quoteDec  = error "declarations"
  }

ly :: QuasiQuoter
ly = QuasiQuoter {
    quoteExp  = compile . deLitify
  , quotePat  = error "patterns"
  , quoteType = error "types"
  , quoteDec  = error "declarations"
  }

deriving instance Lift a => Lift (Directive a)
deriving instance Lift AbsSyn
deriving instance Lift BookendedAbsSyn
deriving instance Lift Rule
deriving instance Lift Prod
deriving instance Lift Term
deriving instance Lift Prec

compile :: String -> Q Exp
compile str = case parseYFileContents str of
               Left err -> error ("y" ++ ':' : err)
               Right bas -> lift bas