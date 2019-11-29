
module Norm (
  norm, Eff, Sem,
  env0, normalize, Counts(..),
  ) where

import Ast (Bin,Exp(..),Prim1(..),Prim4(..),Base)
import Control.Monad (ap,liftM)
import Data.Map.Strict (Map)
import Eval(Value(VBase))
import Prelude hiding (lookup)
import qualified Ast
import qualified Data.Map.Strict as Map
import qualified Eval


noLets :: Bool
noLets = False -- TODO: flag?

normalize :: Env -> Exp -> IO (Either String (Exp,Counts))
normalize env exp = run env (norm exp >>= reify)


type Env = Map String (Eff Sem)

opaque :: String -> (String, Eff Sem)
opaque name = (name, return $ Syntax $ EVar name)

env0 :: Env
env0 = Map.fromList
  [ ("primInt2String", norm $ Ast.prim1 I2S)
  , ("error", norm $ Ast.prim1 PrimErr)
  , opaque "noinline"
--  , opaque "error"
  ]


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

  EBin bin e1 e2 -> do
    s1 <- norm e1
    s2 <- norm e2
    binop bin s1 s2

  EPrim1 prim e1 -> do
    s1 <- norm e1
    unop prim s1

  EPrim4 prim e1 e2 e3 e4 -> do
    s1 <- norm e1
    s2 <- norm e2
    s3 <- norm e3
    s4 <- norm e4
    prim4op prim s1 s2 s3 s4

  ELet x e1 e2 ->
    norm (EApp (ELam x e2) e1)


apply :: Sem -> Sem -> Eff Sem
apply = \case
  Macro f -> \a -> do
    trackInline
    --Io $ putStr "#"

    if noLets || isAtomic a then f a else do
      arg <- reify a
      n <- Fresh
      let x = "u" <> show n
      continuation <- f (Syntax (EVar x)) >>= reify
      return $ Syntax $ ELet x arg continuation

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
  EBin{}   -> False
  EPrim1{} -> False
  EPrim4{} -> False
  EApp{}   -> False
  ELet{}   -> False


unop :: Prim1 -> Sem -> Eff Sem
unop prim = \case
  SemBase b1 -> eitherToError $ fmap SemBase $ Eval.unop prim (VBase b1)
  s1 -> do
    e1 <- reify s1
    return $ Syntax $ EPrim1 prim e1

binop :: Bin -> Sem -> Sem -> Eff Sem
binop bin s1 s2 = case (s1,s2) of
  (SemBase b1, SemBase b2) -> eitherToError $ fmap SemBase $ Eval.binop bin (VBase b1) (VBase b2)
  _ -> do
    e1 <- reify s1
    e2 <- reify s2
    return $ Syntax $ EBin bin e1 e2

prim4op :: Prim4 -> Sem -> Sem -> Sem -> Sem -> Eff Sem
prim4op prim s1 s2 s3 s4 = case (s1,s2) of
  (SemBase b1, SemBase b2) -> eitherToError $ Eval.prim4op prim (VBase b1) (VBase b2) s3 s4
  _ -> do
    e1 <- reify s1
    e2 <- reify s2
    e3 <- reify s3
    e4 <- reify s4
    return $ Syntax $ EPrim4 prim e1 e2 e3 e4

eitherToError :: Either String Sem -> Eff Sem
eitherToError = \case
  Left s -> Error s
  Right b -> return b


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


newtype Counts = Counts { beta :: Int } deriving Show
