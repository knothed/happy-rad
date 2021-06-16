import Happy.Test
import Happy.Frontend
import Paths_happy_rad
import Data.List

main = do
  dir <- getDataDir
  let workingTests = ["Test.ly", "TestMulti.ly", "TestPrecedence.ly", "bug001.ly", "precedence002.y", "bogus-token.y", "bug002.y", "Partial.ly", "issue91.y", "issue94.y"]
  let customTests = ["EpsilonAnnounce.y"]
  let setup = TestSetup {
    happyExec = "happy-rad",
    defaultTests = workingTests,
    customTests = customTests,
    customDataDir = dir,
    allArguments = ["--rad", "--rad --types"],
    stopOnFailure = False
  }
  test setup