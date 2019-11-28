
module Norm (
  norm, Eff, Sem,
  env0, normalize, Counts(..),
  ) where

import Control.Monad (ap,liftM)
import Data.Map.Strict (Map)
import Eval(Bin,Exp(..),Prim1)
import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map


normalize :: Env -> Exp -> IO (Either String (Exp,Counts))
normalize env exp = run env (norm exp >>= reify)


type Env = Map String (Eff Sem)

env0 :: Env
env0 = foldr Norm.preDefined Map.empty predefined

preDefined :: String -> Env -> Env
preDefined name env = Map.insert name (return $ Syntax $ EVar name) env

predefined :: [String]
predefined = ["noinline","primInt2String","error"]


inlineLam :: Bool
inlineLam = True

norm :: Exp -> Eff Sem
norm = \case
  EBase bv -> return $ Syntax $ EBase bv
  ECon v -> return $ Syntax $ ECon v
  EVar s -> do
    env <- GetEnv
    case Map.lookup s env of
      Nothing -> Error $ "unknown var: " <> s
      Just v -> v

  ELam x body
    | inlineLam -> do
        env <- GetEnv
        return $ Macro $ \arg -> do
          SetEnv (Map.insert x (return arg) env) (norm body)

    | otherwise -> do
        env <- GetEnv
        bodyS <- SetEnv (Map.insert x (return $ Syntax $ EVar x) env) (norm body)
        body' <- reify bodyS
        return $ Syntax $ ELam x body'

  EApp fun arg -> do
    funS <- norm fun
    argS <- norm arg
    apply funS argS

  EBin bin e1 e2 -> do
    s1 <- norm e1
    s2 <- norm e2
    binop bin s1 s2

  EPrim1 prim e1 -> do
    s1 <- norm e1
    unop prim s1

  ELet x e1 e2 ->
    norm (EApp (ELam x e2) e1)


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

run :: Env -> Eff a -> IO (Either String (a,Counts))
run env eff =
  loop s0 env eff >>=
  \case
    Left s -> return $ Left s
    Right (a,State{inlines}) -> return $ Right (a,Counts inlines)

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


newtype Counts = Counts { beta :: Int }
