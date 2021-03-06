{-# LANGUAGE LambdaCase #-}
module Interpreter where

import Data.Maybe (fromJust)
import Data.Int (Int32)

import Lang

boolToInt = \case
  False -> 0
  True -> 1

data StackEntry
  = StackValue Value
  | StackFrame [(Identifier, Value)]
  deriving (Show, Eq)

type Stack = [StackEntry]

getStackValue = \case
  StackValue v -> v
  _ -> error "could not get value"

getCurrentFrame = \case
  [] -> error "no current frame"
  (frame @ StackFrame {}:_) -> frame
  _:t -> getCurrentFrame t

interpret :: (Identifier -> Function) -> Stack -> Instruction -> Stack
interpret funcMap stack = \case
  GetLocal name ->
    let StackFrame locals = getCurrentFrame stack in
    (StackValue $ fromJust $ lookup name locals) : stack
  Const v -> StackValue v:stack
  BinOp op -> applyTwo $ case op of
    Add -> (+)
    Sub -> (-)
    Mul -> (*)
  RelOp op -> applyTwo $ fmap (fmap boolToInt) $ case op of
    Eq -> (==)
    Neq -> (/=)
  If thenInsts elseInsts ->
    case stack of
      v:stack' -> interpretInsts stack' (if getStackValue v /= 0 then thenInsts else elseInsts)
  Call fname ->
    let Function params insts = funcMap fname
        bindings = zip params $ map getStackValue stack
        s:_:stack' = interpretInsts (StackFrame bindings : drop (length params) stack) insts
    in s:stack'
  where
    interpretInsts = foldl (interpret funcMap)
    applyTwo f = case stack of
      StackValue v2: StackValue v1:s -> (StackValue $ f v1 v2) : s
      _ -> error "could not get two values"
