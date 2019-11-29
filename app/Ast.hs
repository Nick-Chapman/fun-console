
module Ast (
  Def(..), Exp(..), Base(..), Bin(..), Prim1(..), Prim4(..),
  prim1, greater, equalI, equalS, addition, subtraction, concatenation,
  ) where

----------------------------------------------------------------------

data Def =
  Def String Exp
  deriving (Show)

data Exp
  = EBase Base
  | EVar String
  | ELam String Exp
  | EBin Bin Exp Exp
  | EPrim1 Prim1 Exp
  | EPrim4 Prim4 Exp Exp Exp Exp
  | EApp Exp Exp
  | ELet String Exp Exp

data Base
  = BNum Int
  | BStr String
  deriving (Eq)

data Bin
  = Add | Sub | Hat
  -- | Gri2 | Eqi2 | Eqs2

data Prim1 = I2S | PrimErr deriving (Show)

data Prim4 = Greater | Eqs4 | Eqi4
  deriving (Show)

----------------------------------------------------------------------

trueE,falseE :: Exp
trueE = ELam "x" (ELam "y" (EVar "x"))
falseE = ELam "x" (ELam "y" (EVar "y"))

greater :: Exp -> Exp -> Exp
greater a b = EPrim4 Greater a b trueE falseE

equalS :: Exp -> Exp -> Exp
equalS a b = EPrim4 Eqs4 a b trueE falseE

equalI :: Exp -> Exp -> Exp
equalI a b = EPrim4 Eqi4 a b trueE falseE

mkBin :: Bin -> Exp
mkBin bin = ELam "x" (ELam "y" (EBin bin (EVar "x") (EVar "y")))

add,sub,hat::Exp
add = mkBin Add
sub = mkBin Sub
hat = mkBin Hat

addition :: Exp -> Exp -> Exp
addition x y = EApp (EApp add x) y

subtraction :: Exp -> Exp -> Exp
subtraction x y = EApp (EApp sub x) y

concatenation :: Exp -> Exp -> Exp
concatenation x y = EApp (EApp hat x) y

prim1 :: Prim1 -> Exp
prim1 prim = let x = "x" in ELam x (EPrim1 prim (EVar x))


----------------------------------------------------------------------

instance Show Exp where
  show = \case
    EBase v -> "(" ++ show v ++ ")"
    EVar s -> s
    EApp e1 e2 -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    ELam s body -> "(\\" ++ s ++ "." ++ show body ++ ")"
    EBin bin e1 e2 -> "(" ++ show e1 ++ show bin ++ show e2 ++ ")"
    EPrim1 prim e1 -> "(" ++ show prim ++ " " ++ show e1 ++ ")"
    EPrim4 prim e1 e2 e3 e4 ->
      "(" ++ show prim ++ " " ++ show e1 ++ " " ++ show e2 ++
      " " ++ show e3 ++ " " ++ show e4 ++ ")"
    ELet x e1 e2 -> "(let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"

instance Show Base where
  show = \case
    BNum i -> show i
    BStr s -> show s

instance Show Bin where
  show = \case
    Add -> "+"
    Sub -> "-"
    Hat -> "^"
--    Gri2 -> ">"
--    Eqi2 -> "=="
--    Eqs2 -> "==="
