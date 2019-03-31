{-# LANGUAGE LambdaCase #-}

module Test where

import Data.Maybe (fromJust)
import Data.Int (Int32)

import Lang
import Interpreter

eq = RelOp Eq
sub = BinOp Sub
mul = BinOp Mul

fac =
  Function ["n"]
  [ GetLocal "n"
  , Const 0
  , eq
  , If
    [ Const 1 ]
    [ GetLocal "n"
    , GetLocal "n"
    , Const 1
    , sub
    , Call "fac"
    , mul
    ]
  ]

-- faculty function where we get local after recursive call
fac2 =
  Function ["n"]
  [ GetLocal "n"
  , Const 0
  , eq
  , If
    [ Const 1 ]
    [ GetLocal "n"
    , Const 1
    , sub
    , Call "fac"
    , GetLocal "n"
    , mul
    ]
  ]

facInc =
  [("fac",
  Function ["r", "i"]
  [ GetLocal "i"
  , Const 0
  , eq
  , If
    [ GetLocal "r" ]
    [ GetLocal "i"
    , Const 1
    , sub
    , GetLocal "i"
    , GetLocal "r"
    , mul
    , Call "fac"
    ]
  ])
  ,("entry",
   Function ["n"]
   [ GetLocal "n"
   , Const 1
   , Call "fac"
   ])]

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

runFunc name f args = interpret (\n -> if n == name then f else error "no such func") (map StackValue args) $ Call name

runEntry fmap args = interpret (fromJust . (`lookup` fmap)) (map StackValue args) $ Call "entry"

getResult = \case [StackValue v] -> v

runFuncName :: [(Identifier, Function)] -> Identifier -> [Value] -> Value
runFuncName fmap name args = getResult $ interpret (fromJust . (`lookup` fmap)) (map StackValue args) $ Call name

test :: Monad m => m ()
test = do
  testFact $ getResult . runFunc "fac" fac . (:[])
  testFact $ getResult . runFunc "fac" fac2 . (:[])
  testFact $ getResult . runEntry facInc . (:[])
