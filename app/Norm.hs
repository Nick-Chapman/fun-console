
module Norm (
  Env, env0, normalize, define, preDefined,
  ) where

import Control.Monad (ap,liftM)
import Data.Map.Strict (Map)
import Eval(Bin,Exp(..),Prim1)
import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map


normalize :: Env -> Exp -> IO (Either String (Exp,InlineCount))
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
  ECon v -> return $ Syntax $ ECon v
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

  EPrim1 prim e1 -> do
    s1 <- eval e1
    unop prim s1

  ELet x e1 e2 ->
    eval (EApp (ELam x e2) e1)


apply :: Sem -> Sem -> Eff Sem
apply = \case
  Macro f -> \a -> do
    trackInline
    --Io $ putStr "#"
    f a
  Syntax fun -> \a -> do
    arg <- reify a
    return $ Syntax $ EApp fun arg


unop :: Prim1 -> Sem -> Eff Sem
unop prim s1 = do
  e1 <- reify s1
  return $ Syntax $ EPrim1 prim e1

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

trackInline :: Eff ()
trackInline = Tick

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
  Tick :: Eff ()
  Io :: IO a -> Eff a


data State = State { fresh :: Int, inlines :: Int }

run :: Env -> Eff a -> IO (Either String (a,InlineCount))
run env eff =
  loop s0 env eff >>=
  \case
    Left s -> return $ Left s
    Right (a,State{inlines}) -> return $ Right (a,InlineCount inlines)

  where
    s0 = State { fresh = 0, inlines = 0 }

    loop :: State -> Env -> Eff a -> IO (Either String (a, State))
    loop s@State{fresh,inlines} env = \case
      Ret x -> return $ Right (x, s)
      Bind e f -> loop s env e >>= \case Left s -> return $ Left s; Right (a,s) -> loop s env (f a)
      Error s -> return $ Left s
      GetEnv -> return $ Right (env, s)
      SetEnv env e -> loop s env e
      Fresh -> return $ Right (fresh, s {fresh = fresh + 1})
      Tick -> return $ Right ((),s {inlines = inlines + 1})
      Io io -> do x <- io; return $ Right (x, s)


newtype InlineCount = InlineCount Int

instance Show InlineCount where show (InlineCount n) = "(inlined:" <> show n <> ")"

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
