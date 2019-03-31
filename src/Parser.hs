module Parser where

import Data.Void
import Data.Char

import Text.Megaparsec
import Text.Megaparsec.Char

import Lang

type Parser a = Parsec Void String a

showLower :: Show a => a -> String
showLower = map toLower . show

names :: (Show a, Enum a, Bounded a) => [(String, a)]
names = (\o -> (showLower o, o)) <$> [minBound..]

pEnum :: (Show a, Enum a, Bounded a) => Parser a
pEnum = choice $ map (\(n, d) -> string n *> pure d) names

pInst :: Parser Instruction
pInst =
  choice [ fmap BinOp pEnum
         , fmap RelOp pEnum
         ]
