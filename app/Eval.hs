
module Eval (eval,Counts(..),count0) where

import Control.Monad.Trans.State.Strict (State, modify)
import Prelude hiding (exp, fail, lookup, pred)
import Ast

type Hopefully = Either String

-- counting steps during evaluation
type Count a = State Counts a

data Counts = Counts
  { adds :: Int
  , subs :: Int
  , hats :: Int
  , apps :: Int
  }

count0 :: Counts
count0 = Counts { adds = 0, subs = 0, hats = 0, apps = 0 }

instance Show Counts where
  show Counts {adds,subs,hats,apps} =
    " (adds:" <> show adds <>
    ", subs:" <> show subs <>
    ", hats:" <> show hats <>
    ", apps:" <> show apps <> ")"

-- Normal order evaluation
eval :: Env -> Exp -> Count Value
eval env = \case
  EBase bv -> return (VBase bv)
  EVar s ->
    case lookup env s of
      Nothing -> return $ VError $ "unknown var: " <> s
      Just (exp, env') -> eval env' exp
  ELam x body -> return $ VFun x body env
  EApp fun arg -> do
    vfun <- eval env fun
    apply vfun (arg, env) -- thunk the arg
  EBin bin e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    binop bin v1 v2
  ELet x e1 e2 ->
    eval env (EApp (ELam x e2) e1)

apply :: Value -> (Exp,Env) -> Count Value
apply = \case
  VBase _ -> \_ -> return $ VError "cant apply a base-value as a function"
  e@(VError _) -> \_ -> return e
  VFun x body env -> \thunk -> do
    trackApp
    eval (extend env x thunk) body
  where
    trackApp = modify $ \c -> c {apps = apps c + 1}

binop :: Bin -> Value -> Value -> Count Value
binop = \case
  Add -> doBin (getNum "+L") (getNum "+R") trackAdd (VBase . BNum . uncurry (+))
  Sub -> doBin (getNum "-L") (getNum "-R") trackSub (VBase . BNum . uncurry (-))
  Hat -> doBin (getStr "^L") (getStr "^R") trackHat (VBase . BStr . uncurry (<>))

doBin :: (Value -> Hopefully a)
      -> (Value -> Hopefully b)
      -> Count ()
      -> ((a,b) -> Value)
      -> Value -> Value -> Count Value
doBin get1 get2 track func = \v1 v2 -> do
  let getBoth = do
        g1 <- get1 v1
        g2 <- get2 v2
        return (g1,g2)
  case getBoth of
    Left s -> return $ VError s
    Right (g1,g2) -> do track; return $ func (g1,g2)

trackAdd, trackSub, trackHat :: Count ()
trackAdd = modify $ \c -> c {adds = adds c + 1}
trackSub = modify $ \c -> c {subs = subs c + 1}
trackHat = modify $ \c -> c {hats = hats c + 1}

getNum :: String -> Value -> Hopefully Int
getNum tag = \case
  VBase (BNum n) -> return n
  VBase (BStr _) -> Left $ tag <> " : expected Num, got String"
  VFun _ _ _ -> Left $ tag <> " : expected Num, got Function"
  VError s -> Left s

getStr :: String -> Value -> Hopefully String
getStr tag = \case
  VBase (BNum _) -> Left $ tag <> " : expected String, got Num"
  VBase (BStr s) -> return s
  VFun _ _ _ -> Left $ tag <> " : expected Str, got Function"
  VError s -> Left s
