
module Norm (
  Env, env0, normalize, define, preDefined,
  ) where

import Control.Monad (ap,liftM)
import Data.Map.Strict (Map)
import Eval(Bin,Exp(..))
import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map


normalize :: Env -> Exp -> IO (Either String Exp)
normalize env exp = run env (eval exp >>= reify)


define :: String -> Exp -> Env -> Env
define name exp env = extend name (eval exp) env

preDefined :: String -> Env -> Env
preDefined name env = extend name (return $ Syntax $ EVar name) env


inlineLam :: Bool
inlineLam = True

eval :: Exp -> Eff Sem
eval = \case
  EBase bv -> return $ Syntax $ EBase bv
  EVar s -> do
    env <- GetEnv
    case lookup env s of
      Nothing -> Error $ "unknown var: " <> s
      Just v -> v

  ELam x body
    | inlineLam -> do
        env <- GetEnv
        return $ Macro $ \arg -> do
          SetEnv (extend x (return arg) env) (eval body)

    | otherwise -> do
        env <- GetEnv
        bodyS <- SetEnv (extend x (return $ Syntax $ EVar x) env) (eval body)
        body' <- reify bodyS
        return $ Syntax $ ELam x body'

  EApp fun arg -> do
    funS <- eval fun
    argS <- eval arg
    apply funS argS

  EBin bin e1 e2 -> do
    s1 <- eval e1
    s2 <- eval e2
    binop bin s1 s2

  ELet x e1 e2 ->
    eval (EApp (ELam x e2) e1)


apply :: Sem -> Sem -> Eff Sem
apply = \case
  Macro f -> \a -> do
    --Io $ putStr "#"
    f a
  Syntax fun -> \a -> do
    arg <- reify a
    return $ Syntax $ EApp fun arg


binop :: Bin -> Sem -> Sem -> Eff Sem
binop bin s1 s2 = do
  e1 <- reify s1
  e2 <- reify s2
  return $ Syntax $ EBin bin e1 e2


data Sem
  = Syntax Exp
  | Macro (Sem -> Eff Sem)

reify :: Sem -> Eff Exp
reify = \case
  Syntax exp -> return exp
  Macro f -> do
    n <- Fresh
    let x = "u" <> show n
    body <- f (Syntax (EVar x)) >>= reify
    return $ ELam x body

----------------------------------------------------------------------

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  GetEnv :: Eff Env
  SetEnv :: Env -> Eff a -> Eff a
  Error :: String -> Eff a
  Fresh :: Eff Int
  Io :: IO a -> Eff a

type State = Int

run :: Env -> Eff a -> IO (Either String a)
run env eff = loop 0 env eff >>= \case Left s -> return $ Left s; Right (a,_) -> return $ Right a
  where
    loop :: State -> Env -> Eff a -> IO (Either String (a, State))
    loop c env = \case
      Ret x -> return $ Right (x, c)
      Bind e f -> loop c env e >>= \case Left s -> return $ Left s; Right (a,c) -> loop c env (f a)
      Error s -> return $ Left s
      GetEnv -> return $ Right (env, c)
      SetEnv env e -> loop c env e
      Fresh -> return $ Right (c, c+1)
      Io io -> do x <- io; return $ Right (x, c)

----------------------------------------------------------------------

type Key = String
type Val = Eff Sem
newtype Env = Env { mapping :: Map Key Val }

env0 :: Env
env0 = Env Map.empty

lookup :: Env -> Key -> Maybe Val
lookup Env{mapping} s = Map.lookup s mapping

extend :: Key -> Val -> Env -> Env
extend k v Env{mapping} = Env $ Map.insert k v mapping
