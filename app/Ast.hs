
module Ast
  ( Def(..)
  , Exp(..)
  , Bin(..)
  , Value(..)
  , Base(..)
  , Env,env0,lookup,extend
  ) where

import Data.List(intercalate)
import Data.Map(Map)
import Prelude hiding (exp, fail, lookup, pred)

import qualified Data.Map as Map

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

data Bin = Add | Sub | Hat

instance Show Bin where
  show = \case
    Add -> "+"
    Sub -> "-"
    Hat -> "^"

data Value
  = VBase Base
  | VError String
  | VFun String Exp Env

data Base
  = BNum Int
  | BStr String

instance Show Base where
  show = \case
    BNum i -> show i
    BStr s -> show s

instance Show Value where
  show = \case
    VBase b -> show b
    VError s -> "error: " <> s
    VFun _ _ _ -> "<closure>"

-- Environment of thunks
newtype Env = Env { mapping :: Map String (Exp,Env) }

instance Show Env where
    show Env{mapping} = "{" <> (intercalate "," $ Map.keys mapping) <> "}"

lookup :: Env -> String -> Maybe (Exp,Env)
lookup Env{mapping} s = Map.lookup s mapping

extend :: Env -> String -> (Exp,Env) -> Env
extend Env{mapping} s thunk = Env $ Map.insert s thunk mapping

env0 :: Env
env0 = Env $ Map.fromList []
