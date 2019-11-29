
module Eval (
  Value(..), binop, unop, prim4op,
  eval, Eff,
  env0, run, Counts(..), countsWorsen,
  ) where

import Ast
import Control.Monad (ap,liftM,join)
import Data.IORef
import Data.Map.Strict (Map)
import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map

eval :: Exp -> Eff Value
eval = \case
  EBase bv -> return (VBase bv)
  EVar s -> do
    env <- GetEnv
    case Map.lookup s env of
      Nothing -> return $ VError $ "unknown var: " <> s
      Just v -> v
  exp@(ELam x body) -> do
    env <- GetEnv
    return $ VFun exp $ \arg -> do
      arg' <- share arg
      SetEnv (Map.insert x arg' env) $ eval body
  EApp fun arg -> do
    vfun <- eval fun
    env <- GetEnv
    apply vfun (SetEnv env $ eval arg)
  EBin bin e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    trackBin bin
    return $ either2error $ fmap VBase $ binop bin v1 v2
  EPrim1 prim e1 -> do
    v1 <- eval e1
    return $ either2error $ fmap VBase $ unop prim v1
  EPrim4 prim e1 e2 e3 e4 -> do
    v1 <- eval e1
    v2 <- eval e2
    v3 <- eval e3
    v4 <- eval e4
    return $ either2error $ prim4op prim v1 v2 v3 v4
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

either2error :: Either String Value -> Value
either2error = \case
  Left s -> VError s
  Right v -> v


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

type Env = Map String (Eff Value)

env0 :: Env
env0 =
  foldr (uncurry Map.insert) Map.empty
  [ ("noinline", return $ VFun (EVar "_noinline_") $ \eff -> eff)
  , ("primInt2String", eval $ prim1 I2S)
  , ("error", eval $ prim1 PrimErr)
  ]

----------------------------------------------------------------------

data Value
  = VBase Base
  | VError String -- TODO: move into Base?
  | VFun Exp (Eff Value -> Eff Value)

instance Show Value where
  show = \case
    VBase b -> show b
    VError s -> "error: " <> s
    --VFun exp _ -> "<function: " <> show exp <> ">"
    VFun _ _ -> "<function>"

apply :: Value -> Eff Value -> Eff Value
apply = \case
  VBase base -> \_ -> return $ VError $ "cant apply a base-value as a function: " <> show base
  e@(VError _) -> \_ -> return e
  VFun _exp f -> \v -> do
    trackApp
    --Io $ putStr "@"
    --Io $ putStrLn $ "@" <> show _exp
    f v
  where


prim4op :: Prim4 -> Value -> Value -> a -> a -> Either String a
prim4op = \case
  Greater -> \v1 v2 v3 v4 -> do
    n1 <- getNum ">-L" v1
    n2 <- getNum ">-R" v2
    return $ if n1 > n2 then v3 else v4

  Eqi4 -> \v1 v2 v3 v4 -> do
    n1 <- getNum "==-L" v1
    n2 <- getNum "==-R" v2
    return $ if n1 == n2 then v3 else v4

  Eqs4 -> \v1 v2 v3 v4 -> do
    n1 <- getStr "===-L" v1
    n2 <- getStr "===-R" v2
    return $ if n1 == n2 then v3 else v4


type Hopefully = Either String

unop :: Prim1 -> Value -> Hopefully Base
unop = \case
  I2S -> doUn (getNum "i2s-arg") (Right . BStr . show)
  PrimErr -> doUn (getStr "error-arg") (\s -> Left $ "error:" <> s)

doUn :: (Value -> Hopefully a)
     -> (a -> Hopefully Base)
     -> Value -> Hopefully Base
doUn get1 func = \v1 -> do
  case get1 v1 of
    Left s -> Left s
    Right arg -> func arg

binop :: Bin -> Value -> Value -> Hopefully Base
binop = \case
  Add -> doBin (getNum "+L") (getNum "+R") (BNum . uncurry (+))
  Sub -> doBin (getNum "-L") (getNum "-R") (BNum . uncurry (-))
  Hat -> doBin (getStr "^L") (getStr "^R") (BStr . uncurry (<>))
--  Gri2 -> doBin (getNum ">L") (getNum ">R") (boolV . uncurry (>))
--  Eqi2 -> doBin (getNum "==L") (getNum "==R") (boolV . uncurry (==))
--  Eqs2 -> doBin (getStr "===L") (getStr "===R") (boolV . uncurry (==))

doBin :: (Value -> Hopefully a)
      -> (Value -> Hopefully b)
      -> ((a,b) -> Base)
      -> Value -> Value -> Hopefully Base
doBin get1 get2 func = \v1 v2 -> do
  let getBoth = do
        g1 <- get1 v1
        g2 <- get2 v2
        return (g1,g2)
  case getBoth of
    --Left s -> Right $ VError s
    Left s -> Left s
    Right (g1,g2) -> Right $ func (g1,g2)


getNum :: String -> Value -> Hopefully Int
getNum tag = \case
  VBase (BNum n) -> Right n
  VBase (BStr _) -> Left $ tag <> " : expected Num, got String"
  VFun _ _ -> Left $ tag <> " : expected Num, got Function"
  VError s -> Left s

getStr :: String -> Value -> Hopefully String
getStr tag = \case
  VBase (BStr s) -> Right s
  VBase (BNum _) -> Left $ tag <> " : expected String, got Num"
  VFun _ _ -> Left $ tag <> " : expected Str, got Function"
  VError s -> Left s


----------------------------------------------------------------------

trackBin :: Bin -> Eff ()
trackBin = \case
  Add -> trackAdd
  Sub -> trackSub
  Hat -> trackHat

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

countsWorsen :: Counts -> Counts -> Bool
countsWorsen c1 c2 =
  adds c2 > adds c1 ||
  subs c2 > subs c1 ||
  hats c2 > hats c1 ||
  apps c2 > apps c1
