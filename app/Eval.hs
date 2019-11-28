
module Eval (
  Def(..), Exp(..), Base(..), Bin(..), Prim1(..),
  Value(..),
  eval, Eff,
  env0, run, Counts(..),
  ) where

import Control.Monad (ap,liftM,join)
import Data.Map.Strict (Map)
import Data.IORef
import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map

----------------------------------------------------------------------

data Def =
  Def String Exp
  deriving (Show)

data Exp
  = EBase Base
  | ECon Value
  | EVar String
  | ELam String Exp
  | EBin Bin Exp Exp
  | EPrim1 Prim1 Exp
  | EApp Exp Exp
  | ELet String Exp Exp

data Base
  = BNum Int
  | BStr String

data Bin = Add | Sub | Hat | Eqi | Eqs

data Prim1 = I2S deriving (Show)

----------------------------------------------------------------------

instance Show Exp where
  show = \case
    EBase v -> "(" ++ show v ++ ")"
    ECon v -> "(" ++ show v ++ ")"
    EVar s -> s
    EApp e1 e2 -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    ELam s body -> "(\\" ++ s ++ "." ++ show body ++ ")"
    EBin bin e1 e2 -> "(" ++ show e1 ++ show bin ++ show e2 ++ ")"
    EPrim1 prim e1 -> "(" ++ show prim ++ show e1 ++ ")"
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
    Eqs -> "==="

----------------------------------------------------------------------

eval :: Exp -> Eff Value
eval = \case
  EBase bv -> return (VBase bv)
  ECon v -> return v
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
    binop bin v1 v2
  EPrim1 prim e1 -> do
    v1 <- eval e1
    unop prim v1

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

type Env = Map String (Eff Value)

env0 :: Env
env0 =
  foldr (uncurry Map.insert) Map.empty
  [ ("noinline", return $ VFun (EVar "_noinline_") $ \eff -> eff)
  , ("primInt2String", return $ Eval.primInt2String)
  ]

primInt2String :: Value
primInt2String = VFun (EVar "_primInt2String_") $ \eff -> do
  v <- eff
  let exp :: Exp = ECon v
  eval $ EPrim1 I2S exp

----------------------------------------------------------------------

data Value
  = VBase Base
  | VError String
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

unop :: Prim1 -> Value -> Eff Value
unop = \case
  I2S -> doUn (getNum "i2s-arg") noTrack (VBase . BStr . show)

noTrack :: Eff ()
noTrack = return ()

doUn :: (Value -> Hopefully a)
     -> Eff ()
     -> (a -> Value)
     -> Value -> Eff Value
doUn get1 track func = \v1 -> do
  case get1 v1 of
    Left s -> return $ VError s
    Right arg -> do track; return $ func arg


binop :: Bin -> Value -> Value -> Eff Value
binop = \case
  Add -> doBin (getNum "+L") (getNum "+R") trackAdd (VBase . BNum . uncurry (+))
  Sub -> doBin (getNum "-L") (getNum "-R") trackSub (VBase . BNum . uncurry (-))
  Hat -> doBin (getStr "^L") (getStr "^R") trackHat (VBase . BStr . uncurry (<>))
  Eqi -> doBin (getNum "==L") (getNum "==R") noTrack (boolV . uncurry (==))
  Eqs -> doBin (getStr "===L") (getStr "===R") noTrack (boolV . uncurry (==))

boolV :: Bool -> Value
boolV b = VFun (EVar "_if1_") $ \t -> return $ VFun (EVar "_if2_") $ \f -> if b then t else f

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
  VFun _ _ -> Left $ tag <> " : expected Num, got Function"
  VError s -> Left s

getStr :: String -> Value -> Hopefully String
getStr tag = \case
  VBase (BStr s) -> Right s
  VBase (BNum _) -> Left $ tag <> " : expected String, got Num"
  VFun _ _ -> Left $ tag <> " : expected Str, got Function"
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
