module Lang where

import Data.Int (Int32)

type Identifier = String
type Value = Int32

data BinOp
  = Add
  | Sub
  | Mul

data RelOp
  = Eq
  | Neq

data Instruction
  = GetLocal Identifier
  | Const Value
  | BinOp BinOp
  | RelOp RelOp
  | If [Instruction] [Instruction]
  | Loop [Instruction]
  | Call Identifier

data Function
  = Function [Identifier] [Instruction]
