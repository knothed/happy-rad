module Main where

import FrontendCLI
import MiddleendCLI
import BackendCLI
import RADBackendCLI
import System.IO
import System.Exit
import System.Environment
import OptionParsing
import Control.Monad.Trans.Except
import Control.Monad.Except
import System.Console.GetOpt

extInfo :: [OptDescr RADBackendFlag]
extInfo = argInfoRad

main = do
    options <- parseOptions extInfo =<< getArgs
    grammar <- try $ runFrontend (frontendOpts options)
    (action, goto, lr1Items, unused_rules) <- runMiddleend (middleendOpts options) grammar
    if isRAD (exts options) then
      let radOpts = radFlagsToOpts (BackendCLI.outFile $ backendOpts options) (exts options) in
      runRADBackend radOpts grammar action goto lr1Items unused_rules
      else runBackend (backendOpts options) grammar action goto

try :: IO (Either String a) -> IO a
try f = do
    result <- f
    case result of
      Left err -> liftIO $ die err
      Right a -> return a
    where
        die s = hPutStr stderr s >> exitWith (ExitFailure 1)