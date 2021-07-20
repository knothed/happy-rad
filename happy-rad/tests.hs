import Happy.Test
import Happy.Frontend
import Paths_happy_rad
import Data.List

main = do
  dir <- getDataDir
  -- These tests currently fail due to one of these three reasons:
   -- %monad directive, but no %lexer directive (not yet supported by code-gen)
   -- an '{%%' or '{%^' action (not yet supported by code-gen)
   -- using typeclasses. These will not be supported as they interfere with our typing system.
  let failingTests = ["issue95.y", "test_rules.y", "monaderror.y", "monaderror-explist.y", "typeclass_monad001.y", "typeclass_monad002.ly", "typeclass_monad_lexer.y", "monad001.y", "rank2.y"]

  let customTests = ["EpsilonAnnounce.y"]
  let setup = TestSetup {
    happyExec = "happy-rad",
    defaultTests = defaultTestFiles \\ failingTests,
    customTests = customTests,
    customDataDir = dir,
    allArguments = ["--rad --types"], -- some large test files (issue93.y) need type annotations to compile in reasonable time
    stopOnFailure = False
  }
  test setup