{-# LANGUAGE LambdaCase #-}

import Test.HUnit
import Test.Framework (defaultMain, Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import Data.List
import Data.Maybe (fromJust)

import Interpreter
import Lang
import Parser
import Text.Megaparsec

getResult = \case [StackValue v] -> v

runFuncName :: [(Identifier, Function)] -> Identifier -> [Value] -> Value
runFuncName fmap name args = getResult $ interpret (fromJust . (`lookup` fmap)) (map StackValue args) $ Call name

testFile path = buildTest $ do
  content <- readFile path
  let (functions, scen) = either (error . show) id $ runParser pprogScenario "" content
  let scenarioToTest (name, args, result) =
        TestLabel (name ++ "(" ++ intercalate ", " (map show args) ++ ") = " ++ show result)
        (runFuncName functions name args ~?= result)
  pure $ testGroup path $ concatMap (hUnitTestToTests . scenarioToTest) scen

allTests = [testFile "fac.wal"]

main :: IO ()
main = defaultMain allTests
