
module Ast (
  Def(..),
  Exp(..),
  Base(..),
  Prim1(..),
  Prim2(..),
  Prim3(..),
  env,
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

----------------------------------------------------------------------

data Def =
  Def String Exp
  deriving (Show)

data Exp
  = EBase Base
  | EVar String
  | ELam String Exp
  | EApp Exp Exp
  | ELet String Exp Exp

data Base
  = BNum Int
  | BStr String
  | BBool Bool
  | BPrim1 Prim1
  | BPrim2 Prim2
  | BPrim3 Prim3
  deriving (Eq)

data Prim2
  = Add | Sub | Mul | Hat
  | Eqi2 | Eqs2
  | Less2 | Leq2
  | Greater2 | Geq2
  deriving (Eq,Show)

data Prim1 = I2S | PrimErr deriving (Eq,Show)

data Prim3 = If3 deriving (Eq,Show)

----------------------------------------------------------------------

env :: Map String Exp
env = Map.fromList
  [ ("+", Ast.add)
  , ("-", Ast.sub)
  , ("*", Ast.mul)
  , ("^", Ast.hat)

  , ("==", Ast.eqi)
  , ("===", Ast.eqs)
  , ("int2string", Ast.int2string)
  , ("error", Ast.primErr)

  , ("true", Ast.trueE)
  , ("false", Ast.falseE)
  , ("if", Ast.if3)

  , ("<", Ast.less)
  , ("<=", Ast.leq)

  -- TODO: code as non-primitives (need parser support to use symbolic identifiers in parens)
  , (">", Ast.greater)
  , (">=", Ast.geq)

  ]

----------------------------------------------------------------------

trueE,falseE :: Exp
eqi,eqs,greater,less,leq,geq :: Exp
add,sub,mul,hat :: Exp
primErr,int2string :: Exp
if3 :: Exp

add = EBase $ BPrim2 Add
sub = EBase $ BPrim2 Sub
mul = EBase $ BPrim2 Mul
hat = EBase $ BPrim2 Hat

eqi = EBase $ BPrim2 Eqi2
eqs = EBase $ BPrim2 Eqs2
less = EBase $ BPrim2 Less2
leq = EBase $ BPrim2 Leq2
greater = EBase $ BPrim2 Greater2
geq = EBase $ BPrim2 Geq2

trueE = EBase $ BBool True
falseE = EBase $ BBool False

primErr    = EBase $ BPrim1 PrimErr
int2string = EBase $ BPrim1 I2S

if3 = EBase $ BPrim3 If3

----------------------------------------------------------------------

-- TODO: reduce brackets, as this is shown when normalizing
instance Show Exp where
  show = \case
    EBase v -> "(" ++ show v ++ ")"
    EVar s -> s
    ELam s body -> "(\\" ++ s ++ "." ++ show body ++ ")"
    EApp e1 e2 -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    ELet x e1 e2 -> "(let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"

instance Show Base where
  show = \case
    BNum i -> show i
    BStr s -> show s
    BBool b -> show b
    BPrim1 prim -> show prim
    BPrim2 prim -> show prim
    BPrim3 prim -> show prim

{-instance Show Prim2 where
  show = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Hat -> "^"
    Eqi2 -> "=="
    Eqs2 -> "==="
    Less2 -> "<"
    Leq2 -> "<="
-}
