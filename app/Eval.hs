{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Eval (eval,Counts(..),count0) where

import Control.Monad.Trans.State.Strict (State, modify)
import Prelude hiding (exp, fail, lookup, pred)
import Ast

type Hopefully = Either String

-- counting steps during evaluation
type Count a = State Counts a

data Counts =
    Counts
        { adds :: Int
        , hats :: Int
        , apps :: Int
        }

count0 :: Counts
count0 = Counts { adds = 0, hats = 0, apps = 0 }

instance Show Counts where
    show Counts {adds,hats,apps} =
        " (adds:" <> show adds <> ", hats:" <> show hats <> ", apps:" <> show apps <> ")"

-- Normal order evaluation
eval :: Env -> Exp -> Count Value
eval env =
    \case
        EConst v -> return v
        EVar s ->
            case lookup env s of
                Nothing -> return $ VError $ "unknown var: " <> s
                Just (exp, env') -> eval env' exp
        ELam x body -> return $ VFun x body env
        EApp fun arg -> do
            vfun <- eval env fun
            apply vfun (arg, env) -- thunk the arg
        ENum i -> return $ VNum i
        EAdd e1 e2 -> do
            v1 <- eval env e1
            v2 <- eval env e2
            addV v1 v2
        EStr s -> return $ VStr s
        EHat e1 e2 -> do
            v1 <- eval env e1
            v2 <- eval env e2
            hatV v1 v2
        ELet x e1 e2 ->
            eval env (EApp (ELam x e2) e1)

apply :: Value -> (Exp,Env) -> Count Value
apply = \case
    VNum _ -> \_ -> return $ VError "cant apply a number as a function"
    VStr _ -> \_ -> return $ VError "cant apply a string as a function"
    e@(VError _) -> \_ -> return e
    VFun x body env -> \thunk -> do
        trackApp
        eval (extend env x thunk) body
  where
     trackApp = modify $ \c -> c {apps = apps c + 1}

addV :: Value -> Value -> Count Value
addV v1 v2 = do
    let maybeAdd = do
            n1 <- getNum "add-arg-1" v1
            n2 <- getNum "add-arg-2" v2
            return (n1,n2)
    case maybeAdd of
        Left s -> return $ VError s
        Right (n1,n2) -> do trackAdd; return $ VNum $ n1+n2
  where
    trackAdd = modify $ \c -> c {adds = adds c + 1}

hatV :: Value -> Value -> Count Value
hatV v1 v2 = do
    let maybeHat = do
            s1 <- getStr "hat-arg-1" v1
            s2 <- getStr "hat-arg-2" v2
            return (s1,s2)
    case maybeHat of
        Left s -> return $ VError s
        Right (s1,s2) -> do trackHat; return $ VStr $ s1 <> s2
  where
    trackHat = modify $ \c -> c {hats = hats c + 1}


getNum :: String -> Value -> Hopefully Int
getNum tag = \case
    VNum n -> return n
    VStr _ -> Left $ tag <> " : expected Num, got String"
    VFun _ _ _ -> Left $ tag <> " : expected Num, got Function"
    VError s -> Left s

getStr :: String -> Value -> Hopefully String
getStr tag = \case
    VNum _ -> Left $ tag <> " : expected String, got Num"
    VStr s -> return s
    VFun _ _ _ -> Left $ tag <> " : expected Str, got Function"
    VError s -> Left s

