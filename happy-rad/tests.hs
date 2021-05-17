import Test
import Paths_happy_rad
import Frontend
import Data.List

main = do
  dir <- getDataDir
  let tests = if isBootstrapped then defaultTestFiles ++ bootstrapTestFiles else defaultTestFiles
  let setup = TestSetup {
    happyExec = "happy-rad",
    defaultTests = tests,
    customTests = [],
    customDataDir = dir,
    allArguments = ["--rad"],
    stopOnFailure = False
  }
  test setup