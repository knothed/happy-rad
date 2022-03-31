{-# LANGUAGE DoAndIfThenElse #-}

-----------------------------------------------------------------------------
The main driver.

(c) 1993-2003 Andy Gill, Simon Marlow
GLR amendments (c) University of Durham, Ben Medlock 2001
-----------------------------------------------------------------------------

> module Parse where

Path settings auto-generated by Cabal:

> import Happy.CodeGen.Common.Options
> import Happy.Grammar
> import Happy.Frontend
> import Happy.Frontend.AbsSyn
> import Happy.Frontend.Mangler
> import Happy.Frontend.PrettyGrammar
> import Happy.Backend.LALR
> import Happy.Backend.LALR.Target (Target(..))
> import Happy.Backend.LALR.ProduceCode (produceParser)
> import Happy.Backend.GLR
> import Happy.Backend.GLR.ProduceCode
> import Happy.Backend.RAD
> import qualified Happy.Backend.RAD.CLI as RADCLI
> import Happy.Tabular
> import Happy.Tabular.Info (genInfoFile)

> import System.Console.GetOpt
> import Control.Monad ( liftM, when )
> import System.Environment
> import System.Exit (exitWith, ExitCode(..))
> import Data.Char
> import System.IO
> import Data.List( isSuffixOf )
> import Data.Version ( showVersion )
> import TheAbssyn
> import Language.Haskell.TH

> genParser :: Q [Dec]
> genParser = do

>       (BookendedAbsSyn hd abssyn tl) <- pure abssyn -- case parseYFileContents file of

Mangle the syntax into something useful.

>       let (g, common_options) = case {-# SCC "Mangler" #-} mangler "none" abssyn of
>               Left  s  -> error (unlines s ++ "\n")
>               Right gd -> gd

>       let select_reductions = select_first_reduction

>       let tables      = genTables select_reductions g
>           sets        = lr0items tables
>           lainfo      = (la_prop tables, la_spont tables)
>           la          = lookaheads tables
>           goto        = gotoTable tables
>           action      = actionTable tables
>           (conflictArray,(sr,rr)) = conflicts tables

>       let (unused_rules, unused_terminals) = redundancies tables


Branch off to RAD parser production

>       let opts = RADCLI.parseFlags [] "none"
>       runRADBackend opts g hd tl common_options action goto (lr1items tables) unused_rules genCodeQ
