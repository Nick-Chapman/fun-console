
module Eval (
  Def(..), Exp(..), Base(..), Bin(..),
  Value(VError), Counts(..),
  Env, env0,
  Eff, eval, extend, run,
  ) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import Prelude hiding (lookup)
import qualified Data.Map as Map

----------------------------------------------------------------------

data Def =
  Def String Exp
  deriving (Show)

data Exp
  = EBase Base
  | EVar String
  | ELam String Exp
  | EBin Bin Exp Exp
  | EApp Exp Exp
  | ELet String Exp Exp

data Base
  = BNum Int
  | BStr String

data Bin = Add | Sub | Hat | Eqi

----------------------------------------------------------------------

instance Show Exp where
  show = \case
    EBase v -> "(" ++ show v ++ ")"
    EVar s -> s
    EApp e1 e2 -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    ELam s body -> "(\\" ++ s ++ "." ++ show body ++ ")"
    EBin bin e1 e2 -> "(" ++ show e1 ++ show bin ++ show e2 ++ ")"
    ELet x e1 e2 -> "(let " ++ show x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"

instance Show Base where
  show = \case
    BNum i -> show i
    BStr s -> show s

instance Show Bin where
  show = \case
    Add -> "+"
    Sub -> "-"
    Hat -> "^"
    Eqi -> "=="

----------------------------------------------------------------------

eval :: Exp -> Eff Value
eval = \case
  EBase bv -> return (VBase bv)
  EVar s -> do
    env <- GetEnv
    case lookup env s of
      Nothing -> return $ VError $ "unknown var: " <> s
      Just v -> v
  ELam x body -> do
    env <- GetEnv
    return $ VFun $ \v -> do
      SetEnv (extend x v env) $ eval body
  EApp fun arg -> do
    vfun <- eval fun
    env <- GetEnv
    apply vfun (SetEnv env $ eval arg)
  EBin bin e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    binop bin v1 v2
  ELet x e1 e2 ->
    eval (EApp (ELam x e2) e1)

----------------------------------------------------------------------

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  GetEnv :: Eff Env
  SetEnv :: Env -> Eff a -> Eff a
  Tick :: (Counts -> Counts) -> Eff ()
  Io :: IO a -> Eff a

run :: Env -> Eff a -> IO (a, Counts)
run = loop counts0
  where
    loop :: Counts -> Env -> Eff a -> IO (a,Counts)
    loop c env = \case
      Ret x -> return (x,c)
      Bind e f -> do (v,c) <- loop c env e; loop c env (f v)
      GetEnv -> return (env,c)
      SetEnv env e -> loop c env e
      Tick f -> return ((),f c)
      Io io -> do v <- io; return (v,c)

----------------------------------------------------------------------

newtype Env = Env { mapping :: Map String (Eff Value) }

env0 :: Env
env0 = Env Map.empty

lookup :: Env -> String -> Maybe (Eff Value)
lookup Env{mapping} s = Map.lookup s mapping

extend :: String -> Eff Value -> Env -> Env
extend k v Env{mapping} = Env $ Map.insert k v mapping

----------------------------------------------------------------------

data Value
  = VBase Base
  | VError String
  | VFun (Eff Value -> Eff Value)

instance Show Value where
  show = \case
    VBase b -> show b
    VError s -> "error: " <> s
    VFun _ -> "<function>"

apply :: Value -> Eff Value -> Eff Value
apply = \case
  VBase _ -> \_ -> return $ VError "cant apply a base-value as a function"
  e@(VError _) -> \_ -> return e
  VFun f -> \v -> do
    trackApp
    --Io $ putStr "@"
    f v
  where

binop :: Bin -> Value -> Value -> Eff Value
binop = \case
  Add -> doBin (getNum "+L") (getNum "+R") trackAdd (VBase . BNum . uncurry (+))
  Sub -> doBin (getNum "-L") (getNum "-R") trackSub (VBase . BNum . uncurry (-))
  Hat -> doBin (getStr "^L") (getStr "^R") trackHat (VBase . BStr . uncurry (<>))
  Eqi -> doBin (getNum "==L") (getNum "==R") (return ()) (boolV . uncurry (==))

boolV :: Bool -> Value
boolV b = VFun $ \t -> return $ VFun $ \f -> if b then t else f

type Hopefully = Either String

doBin :: (Value -> Hopefully a)
      -> (Value -> Hopefully b)
      -> Eff ()
      -> ((a,b) -> Value)
      -> Value -> Value -> Eff Value
doBin get1 get2 track func = \v1 v2 -> do
  let getBoth = do
        g1 <- get1 v1
        g2 <- get2 v2
        return (g1,g2)
  case getBoth of
    Left s -> return $ VError s
    Right (g1,g2) -> do track; return $ func (g1,g2)

getNum :: String -> Value -> Hopefully Int
getNum tag = \case
  VBase (BNum n) -> Right n
  VBase (BStr _) -> Left $ tag <> " : expected Num, got String"
  VFun _ -> Left $ tag <> " : expected Num, got Function"
  VError s -> Left s

getStr :: String -> Value -> Hopefully String
getStr tag = \case
  VBase (BStr s) -> Right s
  VBase (BNum _) -> Left $ tag <> " : expected String, got Num"
  VFun _ -> Left $ tag <> " : expected Str, got Function"
  VError s -> Left s

----------------------------------------------------------------------

trackAdd, trackSub, trackHat, trackApp :: Eff ()
trackAdd = Tick $ \c -> c {adds = adds c + 1}
trackSub = Tick $ \c -> c {subs = subs c + 1}
trackHat = Tick $ \c -> c {hats = hats c + 1}
trackApp = Tick $ \c -> c {apps = apps c + 1}

data Counts = Counts
  { adds :: Int
  , subs :: Int
  , hats :: Int
  , apps :: Int
  }

counts0 :: Counts
counts0 = Counts { adds = 0, subs = 0, hats = 0, apps = 0 }

instance Show Counts where
  show Counts {adds,subs,hats,apps} =
    " (adds:" <> show adds <>
    ", subs:" <> show subs <>
    ", hats:" <> show hats <>
    ", apps:" <> show apps <> ")"
