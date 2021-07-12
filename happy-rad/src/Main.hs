module Main where

import qualified Happy.Frontend.CLI as FrontendCLI
import qualified Happy.Middleend.CLI as MiddleendCLI
import qualified Happy.Backend.CLI as BackendCLI
import qualified Happy.Backend.RAD.CLI as RADCLI
import Happy.Backend
import Happy.Backend.RAD
import Happy.Backend.RAD.CLI
import Happy.Core.OptionParsing
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Monad.Trans.Except
import Control.Monad.Except
import Paths_happy_rad

-- Flag conglomerate
data HappyFlag = Frontend FrontendCLI.Flag | Middleend MiddleendCLI.Flag | Backend BackendCLI.Flag | RAD RADCLI.Flag deriving Eq

as :: Functor f => [f a] -> (a -> b) -> [f b]
a `as` b = map (fmap b) a

allOptions :: [OptDescr HappyFlag]
allOptions = FrontendCLI.options `as` Frontend  ++ MiddleendCLI.options `as` Middleend ++ BackendCLI.options `as` Backend ++ RADCLI.optionsWithoutOutfile `as` RAD

getFrontend :: [HappyFlag] -> [FrontendCLI.Flag]
getMiddleend :: [HappyFlag] -> [MiddleendCLI.Flag]
getBackend :: [HappyFlag] -> [BackendCLI.Flag]
getRad :: [HappyFlag] -> [RADCLI.Flag]
getFrontend flags = [a | Frontend a <- flags]
getMiddleend flags = [a | Middleend a <- flags]
getBackend flags = [a | Backend a <- flags]
getRad flags = [a | RAD a <- flags]

-- Main
main :: IO ()
main = do
  let sortedOpts = beginOptionsWith "oip" allOptions -- outfile, info, pretty
  (flags, freeOpts) <- parseOptions sortedOpts version =<< getArgs
  filename <- requireUnnamedArgument freeOpts sortedOpts DieUsage0 DieUsageMult

  basename <- FrontendCLI.getBaseName filename
  grammar <- try $ FrontendCLI.parseAndRun (getFrontend flags) filename basename

  (action, goto, items, unused_rules) <- MiddleendCLI.parseAndRun (getMiddleend flags) filename basename grammar

  backend <- BackendCLI.parseFlags (getBackend flags) basename
  let radFlags = getRad flags ++ [OptRAD_Outfile $ Happy.Backend.outFile backend] -- Pass outfile from backend to rad-backend

  case RADCLI.parseFlags radFlags basename of
    Just rad -> runRADBackend rad grammar action goto items unused_rules
    Nothing -> runBackend backend grammar action goto

try :: IO (Either String a) -> IO a
try f = do
  result <- f
  case result of
    Left err -> liftIO $ die err
    Right a -> return a