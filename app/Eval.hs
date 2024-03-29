
module Eval (
  Value(..), prim1op, prim2op, prim3op,
  eval, Eff,
  env0, run, Counts(..), countsWorsen,
  ) where

import Ast(Exp(..),Base(..),Prim1(..),Prim2(..),Prim3(..))
import qualified Ast
import Control.Monad (ap,liftM,join)
import Data.IORef
import Data.Map.Strict (Map)
import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map

----------------------------------------------------------------------

type Env = Map String (Eff Value)

env0 :: Env
env0 =
  Map.fromList [ ("noinline", return $ VFun $ \_ eff -> eff) ]
  <> Map.map eval Ast.env

----------------------------------------------------------------------

eval :: Exp -> Eff Value
eval = \case
  EBase bv -> return (VBase bv)
  EVar s -> do
    env <- GetEnv
    case Map.lookup s env of
      Nothing -> return $ VError $ "unknown var: " <> s
      Just v -> v
  _exp@(ELam x body) -> do
    env <- GetEnv
    return $ VFun $ \_argE arg -> do
      trackApp
      --Io $ putStr "@"
      --Io $ putStrLn $ show _exp <> " @ " <> show _argE
      arg' <- share arg
      SetEnv (Map.insert x arg' env) $ eval body
  EApp fun arg -> do
    vfun <- eval fun
    env <- GetEnv
    apply vfun arg (SetEnv env $ eval arg)
  ELet x rhs body -> do
--    eval (EApp (ELam x body) rhs)
    v <- share (eval rhs)
    env <- GetEnv
    SetEnv (Map.insert x v env) $ eval body

share :: Eff Value -> Eff (Eff Value)
share arg = do
  r <- Io $ newIORef undefined
  Io $ writeIORef r $ do
    v <- arg
    Io $ writeIORef r (return v)
    return v
  return $ join $ Io $ readIORef r

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

data Value
  = VBase Base
  | VError String -- TODO: move into Base?
  | VFun (Exp -> Eff Value -> Eff Value)

instance Show Value where
  show = \case
    VBase b -> show b
    VError s -> "error: " <> s
    VFun{} -> "<function>"

apply :: Value -> Exp -> Eff Value -> Eff Value
apply = \case

  VBase (BPrim1 prim) -> \_ arg1 -> do
    v1 <- arg1
    --trackPrim1 prim
    return $ either2error $ fmap VBase $ prim1op prim v1

  VBase (BPrim2 prim) -> \_ arg1 ->
    return $ VFun $ \_ arg2 -> do
      v1 <- arg1
      v2 <- arg2
      trackPrim2 prim
      return $ either2error $ fmap VBase $ prim2op prim v1 v2

  VBase (BPrim3 prim) -> \_ arg1 ->
    return $ VFun $ \_ arg2 -> do
      return $ VFun $ \_ arg3 -> do
        v1 <- arg1
--        v2 <- arg2
--        v3 <- arg3
        --trackPrim3 prim
        --e <- prim3op prim v1 arg2 arg3
        --return $ either2error e
        case prim3op prim v1 arg2 arg3 of
          Left s -> return $ VError s
          Right eff -> eff

  VBase base -> \_ _ -> return $ VError $ "cant apply a base-value as a function: " <> show base
  e@(VError _) -> \_ _-> return e
  VFun f -> \arg v -> do
    f arg v


either2error :: Either String Value -> Value
either2error = \case
  Left s -> VError s
  Right v -> v


type Hopefully = Either String

prim1op :: Prim1 -> Value -> Hopefully Base
prim1op = \case
  I2S -> doPrim1 (getNum "i2s-arg") (Right . BStr . show)
  PrimErr -> doPrim1 (getStr "error-arg") (\s -> Left $ "user-called-error:" <> s)

doPrim1 :: (Value -> Hopefully a)
     -> (a -> Hopefully Base)
     -> Value -> Hopefully Base
doPrim1 get1 func = \v1 -> do
  case get1 v1 of
    Left s -> Left s
    Right arg -> func arg

prim2op :: Prim2 -> Value -> Value -> Hopefully Base
prim2op = \case
  Add -> doPrim2 (getNum "+L") (getNum "+R") (BNum . uncurry (+))
  Sub -> doPrim2 (getNum "-L") (getNum "-R") (BNum . uncurry (-))
  Mul -> doPrim2 (getNum "*L") (getNum "*R") (BNum . uncurry (*))
  Hat -> doPrim2 (getStr "^L") (getStr "^R") (BStr . uncurry (<>))
  Eqi2 -> doPrim2 (getNum "==L") (getNum "==R") (BBool . uncurry (==))
  Eqs2 -> doPrim2 (getStr "===L") (getStr "===R") (BBool . uncurry (==))
  Less2 -> doPrim2 (getNum "<L") (getNum "<R") (BBool . uncurry (<))
  Leq2 -> doPrim2 (getNum "<=L") (getNum "<=R") (BBool . uncurry (<=))
  Greater2 -> doPrim2 (getNum ">L") (getNum ">R") (BBool . uncurry (>))
  Geq2 -> doPrim2 (getNum ">=L") (getNum ">=R") (BBool . uncurry (>=))

doPrim2 :: (Value -> Hopefully a)
      -> (Value -> Hopefully b)
      -> ((a,b) -> Base)
      -> Value -> Value -> Hopefully Base
doPrim2 get1 get2 func = \v1 v2 -> do
  let getBoth = do
        g1 <- get1 v1
        g2 <- get2 v2
        return (g1,g2)
  case getBoth of
    Left s -> Left s
    Right (g1,g2) -> Right $ func (g1,g2)

prim3op :: Prim3 -> Value -> a -> a -> Either String a
prim3op = \case
  If3 -> \v1 v2 v3 -> do
    b1 <- getBool "if-condition" v1
    return $ if b1 then v2 else v3

getNum :: String -> Value -> Hopefully Int
getNum tag = \case
  VBase (BNum n) -> Right n
  VBase (BStr _) -> Left $ tag <> " : expected Num, got String"
  VBase (BBool _) -> Left $ tag <> " : expected Num, got Bool"
  VBase (BPrim1 _) -> Left $ tag <> " : expected Num, got prim1"
  VBase (BPrim2 _) -> Left $ tag <> " : expected Num, got prim2"
  VBase (BPrim3 _) -> Left $ tag <> " : expected Num, got prim3"
  VFun _ -> Left $ tag <> " : expected Num, got Function"
  VError s -> Left s

getStr :: String -> Value -> Hopefully String
getStr tag = \case
  VBase (BNum _) -> Left $ tag <> " : expected String, got Num"
  VBase (BStr s) -> Right s
  VBase (BBool _) -> Left $ tag <> " : expected String, got Bool"
  VBase (BPrim1 _) -> Left $ tag <> " : expected String, got prim1"
  VBase (BPrim2 _) -> Left $ tag <> " : expected String, got prim2"
  VBase (BPrim3 _) -> Left $ tag <> " : expected String, got prim3"
  VFun _ -> Left $ tag <> " : expected Str, got Function"
  VError s -> Left s

getBool :: String -> Value -> Hopefully Bool
getBool tag = \case
  VBase (BNum _) -> Left $ tag <> " : expected Bool, got Num"
  VBase (BStr _) -> Left $ tag <> " : expected Bool, got String"
  VBase (BBool b) -> Right b
  VBase (BPrim1 _) -> Left $ tag <> " : expected Bool, got prim1"
  VBase (BPrim2 _) -> Left $ tag <> " : expected Bool, got prim2"
  VBase (BPrim3 _) -> Left $ tag <> " : expected Bool, got prim3"
  VFun _ -> Left $ tag <> " : expected Bool, got Function"
  VError s -> Left s

----------------------------------------------------------------------

trackPrim2 :: Prim2 -> Eff ()
trackPrim2 = \case
  Add -> trackAdd
  Sub -> trackSub
  Mul -> trackMul
  Hat -> trackHat
  Eqi2 -> trackEqi
  _ -> return () -- TODO: track everything

trackAdd, trackSub, trackMul, trackHat, trackApp, trackEqi :: Eff ()
trackAdd = Tick $ \c -> c {adds = adds c + 1}
trackSub = Tick $ \c -> c {subs = subs c + 1}
trackMul = Tick $ \c -> c {muls = muls c + 1}
trackHat = Tick $ \c -> c {hats = hats c + 1}
trackEqi = Tick $ \c -> c {eqis = eqis c + 1}
trackApp = Tick $ \c -> c {apps = apps c + 1}

data Counts = Counts
  { adds :: Int
  , subs :: Int
  , muls :: Int
  , hats :: Int
  , eqis :: Int
  , apps :: Int
  }

counts0 :: Counts
counts0 = Counts { adds = 0, subs = 0, muls = 0, hats = 0, eqis = 0, apps = 0 }

instance Show Counts where
  show Counts {adds,subs,muls,hats,apps} =
    " (+:" <> show adds <>
    ", -:" <> show subs <>
    ", *:" <> show muls <>
    ", ^:" <> show hats <>
    ", ==:" <> show hats <>
    ", @:" <> show apps <> ")"

countsWorsen :: Counts -> Counts -> Bool
countsWorsen c1 c2 =
  adds c2 > adds c1 ||
  subs c2 > subs c1 ||
  muls c2 > muls c1 ||
  hats c2 > hats c1 ||
  eqis c2 > eqis c1 ||
  apps c2 > apps c1
