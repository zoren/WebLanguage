{-# LANGUAGE LambdaCase #-}

import Data.Maybe
import Data.Int (Int32)
import Data.Coerce

type Identifier = String
type Value = Int32

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
  deriving (Show, Eq)

type Stack = [StackEntry]

getStackValue = \case
  StackValue v -> v
  _ -> error "could not get value"

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

-- faculty function where we get local after recursive call
fac2 =
  Function ["n"]
  [ GetLocal "n"
  , Const 0
  , Eq
  , If
    [ Const 1 ]
    [ GetLocal "n"
    , Const 1
    , Sub
    , Call "fac"
    , GetLocal "n"
    , Mul
    ]
  ]

assert :: (Show a, Eq a, Monad m) => a -> a -> m ()
assert expected found = if expected == found then pure () else error $ "Expected " ++ show expected ++ " but found " ++ show found

wrap32 :: Integer -> Int32
wrap32 i = fromInteger (i `mod` 2^31)

testFact f = do
  assert 1 $ f 0
  assert 1 $ f 1
  assert 120 $ f 5
  assert 3628800 $ f 10
  assert (wrap32 6227020800) $ f 13

test :: Monad m => m ()
test = do
  let runFunc name f args = interpret (\n -> if n == name then f else error "no such func") (map StackValue args) $ Call name
  testFact (\n -> getStackValue $ head $ runFunc "fac" fac [n])
  testFact (\n -> getStackValue $ head $ runFunc "fac" fac2 [n])
