{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Ast(Exp(..),Def(..),Value(..),Env,env0,lookup,extend) where

import Data.List(intercalate)
import Data.Map(Map)
import Prelude hiding (exp, fail, lookup, pred)

import qualified Data.Map as Map

data Def =
    Def String Exp
    deriving (Show)

data Exp
    = EConst Value
    | EVar String
    | ELam String Exp
    | EApp Exp Exp
    | ENum Int
    | EAdd Exp Exp
    | ESub Exp Exp
    | EStr String
    | EHat Exp Exp
    | ELet String Exp Exp

-- simple, fully parenthesized, pretty-printer
instance Show Exp where
    show =
        \case
            EConst v -> "(" ++ show v ++ ")"
            EVar s -> s
            EApp e1 e2 -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"
            ELam s body -> "(\\" ++ s ++ "." ++ show body ++ ")"
            ENum i -> show i
            EAdd e1 e2 -> "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
            ESub e1 e2 -> "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
            EStr s -> show s
            EHat e1 e2 -> "(" ++ show e1 ++ "^" ++ show e2 ++ ")"
            ELet x e1 e2 -> "(let " ++ show x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"

data Value
    = VNum Int
    | VStr String
    | VFun String Exp Env
    | VError String

instance Show Value where
    show = \case
        VNum i -> show i
        VStr s -> show s
        VFun _ _ _ -> "<closure>"
        VError s -> "error: " <> s

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
