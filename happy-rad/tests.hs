import Test
import Paths_happy_rad
import Frontend
import Data.List

main = do
  dir <- getDataDir
  let tests = defaultTestFiles ++ (if isBootstrapped then attributeGrammarTestFiles else [])
  let setup = TestSetup {
    happyExec = "happy-rad",
    defaultTests = tests,
    customTests = [],
    customDataDir = dir,
    allArguments = ["--rad"],
    stopOnFailure = False
  }
  test setup