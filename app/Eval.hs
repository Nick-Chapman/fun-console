
module Eval (
  Env, env0,
  Eff, eval, extend, run
  ) where

import Ast (Exp(..))
import Control.Monad (ap,liftM)
import Data.Map (Map)
import Prelude hiding (lookup)
import Value (Counts,Value(VBase,VError,VFun))
import qualified Data.Map as Map
import qualified Value (Eff,apply,binop,run)


eval :: Exp -> Eff Value
eval = \case
  EBase bv -> return (VBase bv)
  EVar s -> do
    env <- GetEnv
    case lookup env s of
      Nothing -> return $ VError $ "unknown var: " <> s
      Just v -> v
  ELam x body -> do
    embed <- GetEmbed
    return $ VFun $ \v -> do
      embed $ ModEnv (extend x (Lift v)) (eval body)
  EApp fun arg -> do
    vfun <- eval fun
    embed <- GetEmbed
    Lift $ Value.apply vfun (embed (eval arg))
  EBin bin e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    Lift $ Value.binop bin v1 v2
  ELet x e1 e2 ->
    eval (EApp (ELam x e2) e1)


data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  GetEnv :: Eff Env
  ModEnv :: (Env -> Env) -> Eff a -> Eff a
  Lift :: Value.Eff a -> Eff a
  GetEmbed :: Eff (Eff a -> Value.Eff a)

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind


run :: Env -> Eff a -> IO (a, Counts)
run env = Value.run . loop env
  where
    loop :: Env -> Eff a -> Value.Eff a
    loop env = \case
      Ret x -> return x
      Bind e f -> do v <- loop env e; loop env (f v)
      GetEnv -> return env
      ModEnv f e -> loop (f env) e
      Lift c -> c
      GetEmbed -> return (loop env)


newtype Env = Env { mapping :: Map String (Eff Value) }

env0 :: Env
env0 = Env Map.empty

lookup :: Env -> String -> Maybe (Eff Value)
lookup Env{mapping} s = Map.lookup s mapping

extend :: String -> Eff Value -> Env -> Env
extend k v Env{mapping} = Env $ Map.insert k v mapping
