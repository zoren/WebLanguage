module Parser where

import Data.Void
import Data.Int
import Data.Char

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Lang

type Parser a = Parsec Void String a

allMembers :: (Enum a, Bounded a) => [a]
allMembers = [minBound..]

whiteSpace = L.space space1 (L.skipLineComment "// ") empty

lexeme = L.lexeme whiteSpace

str = lexeme . string

parens p = str "(" *> p <* str ")"

pEnum :: (Show a, Enum a, Bounded a) => Parser a
pEnum = choice $ map (\d -> str (map toLower $ show d) *> pure d) allMembers

name = lexeme $ some letterChar

pvalue :: Parser Int32
pvalue = L.decimal

pInst :: Parser Instruction
pInst = choice
  [ BinOp <$> pEnum
  , RelOp <$> pEnum
  , Const <$> (str "const" *> lexeme pvalue)
  , GetLocal <$> (str "local.get" *> name)
  , Call <$> (str "call" *> name)
  , If <$> (str "if" *> pInsts <* str "else") <*> (pInsts <* str "end")
  ]

pInsts = many pInst

pcommaSep p = sepBy p $ str ","

pFunc = Function <$> parens (pcommaSep name) <*> pInsts

pprog = whiteSpace *> many ((,) <$> name <*> pFunc)

pscase = (,,) <$> name <*> parens (pcommaSep pvalue) <* str "=" <*> pvalue

pScenario = many (str "//!" *> pscase <* char '\n')

pprogScenario = (,) <$> pprog <*> pScenario <* eof
