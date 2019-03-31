import           Test.HUnit
import           Test.Framework (defaultMain, Test, testGroup, buildTest)
import           Test.Framework.Providers.HUnit (hUnitTestToTests)

import Parser
import Interpreter
import Text.Megaparsec
import Test

testFile path = buildTest $ do
  content <- readFile path
  let functions = either (error . show) id $ runParser pprog "" content
  pure $ testGroup path $ hUnitTestToTests $ runFuncName functions "fac" [5] ~?= 120

hunitTests = testFile "fac.wal"

allTests = [testFile "fac.wal"]

main :: IO ()
main = defaultMain allTests
