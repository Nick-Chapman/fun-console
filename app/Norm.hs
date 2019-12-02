
module Norm (
  norm, Eff, Sem,
  env0, normalize, Counts(..),
  ) where

import Ast (Exp(..),Base(..),Prim1,Prim2,Prim3)
import Control.Monad (ap,liftM)
import Data.Map.Strict (Map)
import Eval(Value(VBase))
import Prelude hiding (lookup)
import qualified Ast
import qualified Data.Map.Strict as Map
import qualified Eval

noLets :: Bool
noLets = False -- TODO: flag?

normalize :: Env -> Exp -> IO (Either String ((Sem,Exp),Counts))
normalize env exp = run env $ do
  sem <- norm exp
  exp' <- reify sem
  return (sem,exp')

type Env = Map String (Eff Sem)

opaque :: String -> (String, Eff Sem)
opaque name = (name, return $ Syntax $ EVar name)

env0 :: Env
env0 =
  Map.fromList [ opaque "noinline"] <>
  Map.map norm Ast.env

inlineLam :: Bool
inlineLam = True

data Sem
  = Syntax Exp
  | Macro (Sem -> Eff Sem)
  | SemBase Base

reify :: Sem -> Eff Exp
reify = \case
  Syntax exp -> return exp
  SemBase b -> return $ EBase b
  Macro f -> do
    n <- Fresh
    let x = "u" <> show n
    body <- f (Syntax (EVar x)) >>= reify
    return $ ELam x body

norm :: Exp -> Eff Sem
norm = \case

  EBase b -> return $ SemBase b

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

  ELet x e1 e2 ->
    norm (EApp (ELam x e2) e1)


apply :: Sem -> Sem -> Eff Sem
apply = \case
  Macro f -> \a -> do
    TickApp
    --Io $ putStr "#"

    if noLets || isAtomic a then f a else do
      arg <- reify a
      n <- Fresh
      let x = "u" <> show n
      continuation <- f (Syntax (EVar x)) >>= reify
      return $ Syntax $ ELet x arg continuation

  SemBase (BPrim1 prim) -> \arg1 -> do
    prim1op prim arg1

  SemBase (BPrim2 prim) -> \arg1 -> do
    return $ Macro $ \arg2 -> do
      prim2op prim arg1 arg2

  SemBase (BPrim3 prim) -> \arg1 -> do
    return $ Macro $ \arg2 -> do
      return $ Macro $ \arg3 -> do
        prim3op prim arg1 arg2 arg3

  f -> \a -> do
    fun <- reify f
    arg <- reify a
    return $ Syntax $ EApp fun arg

isAtomic :: Sem -> Bool
isAtomic = \case
  Macro _ -> True
  SemBase _ -> True
  Syntax exp -> isAtomicExp exp

isAtomicExp :: Exp -> Bool
isAtomicExp = \case
  EBase{}  -> True
  EVar{}   -> True
  ELam{}   -> False
  EApp{}   -> False
  ELet{}   -> False

prim1op :: Prim1 -> Sem -> Eff Sem
prim1op prim = \case
  SemBase b1 -> eitherToError $ fmap SemBase $ Eval.prim1op prim (VBase b1)
  s1 -> do
    e1 <- reify s1
    return $ Syntax $ EApp (EBase (BPrim1 prim)) e1

prim2op :: Prim2 -> Sem -> Sem -> Eff Sem
prim2op prim s1 s2 = case (s1,s2) of
  (SemBase b1, SemBase b2) -> eitherToError $ fmap SemBase $ Eval.prim2op prim (VBase b1) (VBase b2)
  _ -> do
    e1 <- reify s1
    e2 <- reify s2
    return $ Syntax $ EApp (EApp (EBase $ BPrim2 prim) e1) e2

prim3op :: Prim3 -> Sem -> Sem -> Sem -> Eff Sem
prim3op prim s1 s2 s3 = case s1 of
  SemBase b1 -> eitherToError $ Eval.prim3op prim (VBase b1) s2 s3
  _ -> do
    e1 <- reify s1
    e2 <- reify s2
    e3 <- reify s3
    return $ Syntax $ EApp (EApp (EApp (EBase $ BPrim3 prim) e1) e2) e3

eitherToError :: Either String Sem -> Eff Sem
eitherToError = \case
  Left s -> Error s
  Right b -> return b

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
  TickApp :: Eff ()
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
      TickApp -> return $ Right ((),s {inlines = inlines + 1})
      Io io -> do x <- io; return $ Right (x, s)

newtype Counts = Counts { beta :: Int } deriving Show
