module Lang where

import Data.Int (Int32)

type Identifier = String
type Value = Int32

data BinOp
  = Add
  | Sub
  | Mul
  deriving (Show, Eq, Enum, Bounded)

data RelOp
  = Eq
  | Neq
  deriving (Show, Eq, Enum, Bounded)

type Instructions = [Instruction]

data Instruction
  = BinOp BinOp
  | RelOp RelOp
  | Const Value
  | GetLocal Identifier
  | Call Identifier
  | If Instructions Instructions
  deriving (Show, Eq)

data Function
  = Function [Identifier] Instructions
  deriving (Show, Eq)
