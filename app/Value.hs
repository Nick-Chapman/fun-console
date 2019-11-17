
module Value (
  Counts(..), counts0,
  Value(..), Base(..),
  Count, -- The effect! (runState to run)
  apply,
  Bin(..), binop,
  ) where

import Control.Monad.Trans.State.Strict (State,modify)

data Value
  = VBase Base
  | VError String
  | VFun (Count Value -> Count Value)

data Base
  = BNum Int
  | BStr String

instance Show Base where
  show = \case
    BNum i -> show i
    BStr s -> show s

instance Show Value where
  show = \case
    VBase b -> show b
    VError s -> "error: " <> s
    VFun _ -> "<function>"

apply :: Value -> Count Value -> Count Value
apply = \case
  VBase _ -> \_ -> return $ VError "cant apply a base-value as a function"
  e@(VError _) -> \_ -> return e
  VFun f -> \v -> do
    trackApp
    f v
  where
    trackApp = modify $ \c -> c {apps = apps c + 1}

data Bin = Add | Sub | Hat | Eqi

instance Show Bin where
  show = \case
    Add -> "+"
    Sub -> "-"
    Hat -> "^"
    Eqi -> "=="

binop :: Bin -> Value -> Value -> Count Value
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
  VFun _ -> Left $ tag <> " : expected Num, got Function"
  VError s -> Left s

getStr :: String -> Value -> Hopefully String
getStr tag = \case
  VBase (BNum _) -> Left $ tag <> " : expected String, got Num"
  VBase (BStr s) -> return s
  VFun _ -> Left $ tag <> " : expected Str, got Function"
  VError s -> Left s


-- counting steps during evaluation
type Count a = State Counts a

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