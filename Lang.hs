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

type Instructions = [Instruction]

data Instruction
  = GetLocal Identifier
  | Const Value
  | BinOp BinOp
  | RelOp RelOp
  | Call Identifier
  | If Instructions Instructions

data Function
  = Function [Identifier] Instructions
