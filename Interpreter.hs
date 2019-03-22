{-# LANGUAGE LambdaCase #-}

import Data.Maybe

type Identifier = String

data Instruction
  = GetLocal Identifier
  | Const Int
  | Eq
  | Sub
  | Mul
  | If [Instruction] [Instruction]
  | Call Identifier

data Function
  = Function [Identifier] [Instruction]

interpret funcMap (locals, stack) = \case
  GetLocal name -> updStack $ (fromJust $ lookup name locals) : stack
  Const v -> updStack $ v:stack
  Eq -> applyTwo $ \v1 v2 -> if v1 == v2 then 1 else 0
  Sub -> applyTwo $ flip (-)
  Mul -> applyTwo (*)
  If thenInsts elseInsts ->
    case stack of
      v:s -> foldl (interpret funcMap) (locals, s) (if v == 1 then thenInsts else elseInsts)
  Call fname ->
    let Function params insts = funcMap fname
        bindings = zip params stack
    in foldl (interpret funcMap) (bindings, drop (length params) stack) insts
  where
    updStack ns = (locals, ns)
    applyTwo f = case stack of
      v1:v2:s -> updStack $ (f v1 v2) : s
      _ -> error "could not get two"

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

test = interpret (\case "fac" -> fac) ([], [5]) $ Call "fac"
