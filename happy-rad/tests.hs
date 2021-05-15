import Test
import Paths_happy_rad
import Frontend
import Data.List

main = do
  dir <- getDataDir
  let tests' = if isBootstrapped then defaultTestFiles else defaultTestFiles ++ bootstrapTestFiles
  let tests = delete "monad001.y" tests' -- todo: implement monad-only-parser (without lexer)
  let arguments = ["--rad"] -- ++ defaultArguments
  let setup = TestSetup { happyExec = "happy-rad", defaultTests = tests, customTests = [], customDataDir = dir, allArguments = arguments }
  test setup