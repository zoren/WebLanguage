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
  | GetLocal Identifier
  | Const Value
  | Call Identifier
  | If Instructions Instructions

data Function
  = Function [Identifier] Instructions
