module Main where

import qualified Happy.Frontend.CLI as FrontendCLI
import qualified Happy.Middleend.CLI as MiddleendCLI
import qualified Happy.Backend.CLI as BackendCLI
import qualified Happy.Backend.RAD.CLI as RADBackendCLI
import qualified Happy.Backend.GLR.CLI as GLRBackendCLI
import qualified Happy.Backend as Backend
import qualified Happy.Backend.RAD as RADBackend
import qualified Happy.Backend.GLR as GLRBackend
import Happy.Core.GenUtils
import Happy.Core.OptionParsing
import System.IO
import System.Environment
import System.Console.GetOpt
import Control.Monad.Trans.Except
import Control.Monad.Except
import Paths_happy_rad (version)

-- All flags
data HappyFlag = Frontend FrontendCLI.Flag | Middleend MiddleendCLI.Flag | Backend BackendCLI.Flag | GLRBackend GLRBackendCLI.Flag | RADBackend RADBackendCLI.Flag deriving Eq

as :: Functor f => [f a] -> (a -> b) -> [f b]
a `as` b = map (fmap b) a

getFrontend :: [HappyFlag] -> [FrontendCLI.Flag]
getMiddleend :: [HappyFlag] -> [MiddleendCLI.Flag]
getBackend :: [HappyFlag] -> [BackendCLI.Flag]
getGLRBackend :: [HappyFlag] -> [GLRBackendCLI.Flag]
getRADBackend :: [HappyFlag] -> [RADBackendCLI.Flag]
getFrontend flags = [a | Frontend a <- flags]
getMiddleend flags = [a | Middleend a <- flags]
getBackend flags = [a | Backend a <- flags]
getGLRBackend flags = [a | GLRBackend a <- flags]
getRADBackend flags = [a | RADBackend a <- flags]

-- Stick options togehter from all packages
allOptions :: [OptDescr HappyFlag]
allOptions =
  FrontendCLI.options `as` Frontend ++
  MiddleendCLI.options `as` Middleend ++
  BackendCLI.options `as` Backend ++
  -- Add the "--glr" option. Remove options that are already declared in happy-backend like outfile, template, ghc, debug.
  removeAllOverlaps BackendCLI.options (GLRBackendCLI.characteristicOption : GLRBackendCLI.options) `as` GLRBackend ++
  -- Add the "--rad" option. Remove options that are already declared in happy-backend like outfile.
  -- Also remove possible short-option overlaps with GLR flags.
  (cleanShortOverlaps GLRBackendCLI.options . removeAllOverlaps BackendCLI.options) (RADBackendCLI.characteristicOption : RADBackendCLI.options) `as` RADBackend

-- Main
main :: IO ()
main = do
  let sortedOpts = beginOptionsWith "oip" allOptions -- Order: outfile, info, pretty
  (flags, freeOpts) <- parseOptions sortedOpts version =<< getArgs
  filename <- requireUnnamedArgument freeOpts sortedOpts DieUsage0 DieUsageMult
  basename <- FrontendCLI.getBaseName filename

  grammar <- try $ FrontendCLI.parseAndRun (getFrontend flags) filename basename
  (action, goto, items, unused_rules) <- MiddleendCLI.parseAndRun (getMiddleend flags) filename basename grammar

  -- Backend / GLRBackend / RADBackend switching
  let glrFlags = getGLRBackend flags
  let radFlags = getRADBackend flags
  let isGLR = GLRBackendCLI.hasCharacteristicFlag glrFlags
  let isRAD = RADBackendCLI.hasCharacteristicFlag radFlags
  backendOpts <- BackendCLI.parseFlags (getBackend flags) basename

  case (isGLR, isRAD) of
    (True, True) -> dieHappy "You cannot use GLR and RAD at the same time."
    (True, False) -> GLRBackend.runGLRBackend (createGLROpts glrFlags backendOpts basename) grammar action goto
    (False, True) -> RADBackend.runRADBackend (createRADOpts radFlags backendOpts basename) grammar action goto items unused_rules
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