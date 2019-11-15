
module Eval (
  eval,
  Env, env0, extend
  ) where

import Prelude hiding (lookup)
import Ast (Exp(..))
import Value (Count,Value(..),apply,binop)

import Data.Map (Map)
import qualified Data.Map as Map

-- Normal order evaluation
eval :: Env -> Exp -> Count Value
eval env = \case
  EBase bv -> return (VBase bv)
  EVar s ->
    case lookup env s of
      Nothing -> return $ VError $ "unknown var: " <> s
      Just v -> v
  ELam x body -> return $ VFun $ \v -> eval (extend env x v) body
  EApp fun arg -> do
    vfun <- eval env fun
    apply vfun (eval env arg)
  EBin bin e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    binop bin v1 v2
  ELet x e1 e2 ->
    eval env (EApp (ELam x e2) e1)

newtype Env = Env { mapping :: Map String (Count Value) }

lookup :: Env -> String -> Maybe (Count Value)
lookup Env{mapping} s = Map.lookup s mapping

extend :: Env -> String -> (Count Value) -> Env
extend Env{mapping} s v = Env $ Map.insert s v mapping

env0 :: Env
env0 = Env Map.empty
