
module Ast(
  Def(..),
  Exp(..),
  ) where

import Value(Base,Bin)

data Def =
  Def String Exp
  deriving (Show)

data Exp
  = EBase Base
  | EVar String
  | ELam String Exp
  | EBin Bin Exp Exp
  | EApp Exp Exp
  | ELet String Exp Exp

-- simple, fully parenthesized, pretty-printer
instance Show Exp where
  show = \case
    EBase v -> "(" ++ show v ++ ")"
    EVar s -> s
    EApp e1 e2 -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    ELam s body -> "(\\" ++ s ++ "." ++ show body ++ ")"
    EBin bin e1 e2 -> "(" ++ show e1 ++ show bin ++ show e2 ++ ")"
    ELet x e1 e2 -> "(let " ++ show x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
