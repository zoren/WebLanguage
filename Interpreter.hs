{-# LANGUAGE LambdaCase #-}

import Data.Maybe

type Identifier = String
-- todo make Int32
type Value = Int

data Instruction
  = GetLocal Identifier
  | Const Value
  | Eq
  | Sub
  | Mul
  | If [Instruction] [Instruction]
  | Loop [Instruction]
  | Call Identifier

data Function
  = Function [Identifier] [Instruction]

boolToInt = \case
  False -> 0
  True -> 1

data StackEntry
  = StackValue Value
  | StackFrame [(Identifier, Value)]
  deriving (Show)

type Stack = [StackEntry]

interpret :: (Identifier -> Function) -> Stack -> Instruction -> Stack
interpret funcMap stack = \case
  GetLocal name ->
    let StackFrame locals = currentFrame in
    (StackValue $ fromJust $ lookup name locals) : stack
  Const v -> StackValue v:stack
  Eq -> applyTwo $ \v1 v2 -> boolToInt $ v1 == v2
  Sub -> applyTwo (-)
  Mul -> applyTwo (*)
  If thenInsts elseInsts ->
    case stack of
      v:stack' -> interpretInsts stack' (if getStackValue v /= 0 then thenInsts else elseInsts)
  Call fname ->
    let Function params insts = funcMap fname
        bindings = zip params stackValues
        s:_:stack' = interpretInsts (StackFrame bindings : drop (length params) stack) insts
    in s:stack'
  where
    interpretInsts = foldl (interpret funcMap)
    getCurrentFrame = \case
      [] -> error "no current frame"
      (frame @ StackFrame {}:_) -> frame
      _:t -> getCurrentFrame t
    currentFrame = getCurrentFrame stack
    getStackValue = \case
      StackValue v -> v
      _ -> error "could not get value"
    stackValues = map getStackValue stack
    applyTwo f = case stack of
      StackValue v1: StackValue v2:s -> (StackValue $ f ( v2) ( v1)) : s
      _ -> error "could not get two values"

fac =
  Function ["n"]
  [ GetLocal "n"
  , Const 0
  , Eq
  , If
    [ Const 1 ]
    [ GetLocal "n"
    , GetLocal "n"
    , Const 1
    , Sub
    , Call "fac"
    , Mul
    ]
  ]

test = interpret (\case "fac" -> fac) [StackValue 5] $ Call "fac"

