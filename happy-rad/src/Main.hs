module Main where

import qualified Happy.Frontend.CLI as FrontendCLI
import qualified Happy.Tabular.CLI as TabularCLI
import qualified Happy.Backend.CLI as BackendCLI
import qualified Happy.Backend.RAD.CLI as RADBackendCLI
import qualified Happy.Backend.GLR.CLI as GLRBackendCLI
import qualified Happy.Backend as Backend
import qualified Happy.Backend.RAD as RADBackend
import qualified Happy.Backend.GLR as GLRBackend
import Happy.Grammar.GenUtils
import Happy.CLI.OptionParsing
import System.IO
import System.Environment
import System.Console.GetOpt
import Control.Monad.Trans.Except
import Control.Monad.Except
import Paths_happy_rad (version)

-- Options for switching between backend, rad-backend and glr-backend
useGLROption, useRADOption :: OptDescr TopLevelFlag
useGLROption = Option "l" ["glr"] (NoArg OptGLR) "Generate a GLR parser for ambiguous grammars"
useRADOption = Option "r" ["rad"] (NoArg OptRAD) "generate a recursive ascent-descent parser"
data TopLevelFlag = OptGLR | OptRAD deriving Eq

-- Combine the flags from all the packages
data HappyFlag = TopLevel TopLevelFlag | Frontend FrontendCLI.Flag | Tabular TabularCLI.Flag | Backend BackendCLI.Flag | GLRBackend GLRBackendCLI.Flag | RADBackend RADBackendCLI.Flag deriving Eq

as :: Functor f => [f a] -> (a -> b) -> [f b]
a `as` b = map (fmap b) a

getTopLevel :: [HappyFlag] -> [TopLevelFlag]
getFrontend :: [HappyFlag] -> [FrontendCLI.Flag]
getTabular :: [HappyFlag] -> [TabularCLI.Flag]
getBackend :: [HappyFlag] -> [BackendCLI.Flag]
getGLRBackend :: [HappyFlag] -> [GLRBackendCLI.Flag]
getRADBackend :: [HappyFlag] -> [RADBackendCLI.Flag]
getTopLevel flags = [a | TopLevel a <- flags]
getFrontend flags = [a | Frontend a <- flags]
getTabular flags = [a | Tabular a <- flags]
getBackend flags = [a | Backend a <- flags]
getGLRBackend flags = [a | GLRBackend a <- flags]
getRADBackend flags = [a | RADBackend a <- flags]

-- Stick options togehter from all packages
allOptions :: [OptDescr HappyFlag]
allOptions =
  FrontendCLI.options `as` Frontend ++
  TabularCLI.options `as` Tabular ++
  BackendCLI.options `as` Backend ++
  -- Add the "--glr" option. Remove options that are already declared in happy-backend like outfile, template, ghc, debug.
  [useGLROption] `as` TopLevel ++
  removeAllOverlaps BackendCLI.options GLRBackendCLI.options `as` GLRBackend ++
  -- Add the "--rad" option. Remove options that are already declared in happy-backend like outfile.
  -- Also remove possible short-option overlaps with GLR flags.
  [useRADOption] `as` TopLevel ++
  (cleanShortOverlaps GLRBackendCLI.options . removeAllOverlaps BackendCLI.options) RADBackendCLI.options `as` RADBackend

-- Main
main :: IO ()
main = do
  let sortedOpts = beginOptionsWith "oip" allOptions -- Order: outfile, info, pretty
  (flags, freeOpts) <- parseOptions sortedOpts version =<< getArgs
  filename <- requireUnnamedArgument freeOpts sortedOpts DieUsage0 DieUsageMult
  basename <- FrontendCLI.getBaseName filename

  grammar <- try $ FrontendCLI.parseAndRun (getFrontend flags) filename basename
  (action, goto, items, unused_rules) <- TabularCLI.parseAndRun (getTabular flags) filename basename grammar

  -- Backend / GLRBackend / RADBackend switching
  let useGLR = OptGLR `elem` getTopLevel flags
  let useRAD = OptRAD `elem` getTopLevel flags
  backendOpts <- BackendCLI.parseFlags (getBackend flags) basename

  case (useGLR, useRAD) of
    (True, True) -> dieHappy "You cannot use GLR and RAD at the same time."
    (True, False) -> GLRBackend.runGLRBackend (createGLROpts (getGLRBackend flags) backendOpts basename) grammar action goto
    (False, True) -> RADBackend.runRADBackend (createRADOpts (getRADBackend flags) backendOpts basename) grammar action goto items unused_rules
    (False, False) -> Backend.runBackend backendOpts grammar action goto

-- Fill those glr-options that were removed due to overlap with happy-backend's options
createGLROpts :: [GLRBackendCLI.Flag] -> Backend.BackendArgs -> String -> GLRBackend.GLRBackendArgs
createGLROpts glrFlags backendOpts basename =
  let glrOpts' = GLRBackendCLI.parseFlags glrFlags basename
  in glrOpts' {
    GLRBackend.outFile = Backend.outFile backendOpts,
    GLRBackend.templateDir = Backend.templateDir backendOpts,
    GLRBackend.ghc = Backend.ghc backendOpts,
    GLRBackend.debug = Backend.debug backendOpts
    }

-- Fill those rad-options that were removed due to overlap with happy-backend's options
createRADOpts :: [RADBackendCLI.Flag] -> Backend.BackendArgs -> String -> RADBackend.RADBackendArgs
createRADOpts radFlags backendOpts basename =
  let radOpts' = RADBackendCLI.parseFlags radFlags basename
  in radOpts' {
    RADBackend.outFile = Backend.outFile backendOpts
    }

try :: IO (Either String a) -> IO a
try f = do
  result <- f
  case result of
    Left err -> liftIO $ die err
    Right a -> return a