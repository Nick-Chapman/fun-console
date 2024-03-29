
module Main (main) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Ast (Exp(..))
import Ast (Def(..),Base(..))
import Eval (Value(..),Eff,countsWorsen)
import Parse (parseDef)
import System.Environment (getArgs)
import qualified Eval
import qualified Norm
import qualified System.Console.ANSI as AN
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.History as HL

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  args <- getArgs
  let conf = parseArgs args
  HL.runInputT haskelineSettings $ start conf

data Conf = Conf
  { funFile :: FilePath
  , verbose :: Bool
--  , normCheck :: Bool
  , doNorm :: Bool
  }

defaultConf :: Conf
defaultConf = Conf
  { funFile = ".history"
  , verbose = False
--  , normCheck = False
  , doNorm = True
  }

parseArgs :: [String] -> Conf
parseArgs args = loop args defaultConf
  where
    loop args conf = case args of
      [] -> conf
      "-v":rest -> loop rest $ conf { verbose = True }
--      "-n":rest -> loop rest $ conf { normCheck = True }
      "-nn":rest -> loop rest $ conf { doNorm = False } --"nn" == no-norm
      funFile:rest -> loop rest $ conf { funFile }

start :: Conf -> HL.InputT IO ()
start conf = do
  history <- lift $ readHistory conf
  HL.putHistory history
  env <- lift $ replay conf env0 (HL.historyLines history)
  repl conf 1 env


data Env = Env
  { evaluationEnvS :: EE --slow
  , evaluationEnvF :: EE --fast (uses normalized expressions)
  , normalizationEnv  :: Map String (Norm.Eff Norm.Sem)
  }

type EE = Map String (Eval.Eff Eval.Value)

env0 :: Env
env0 = Env
  { evaluationEnvS = Eval.env0
  , evaluationEnvF = Eval.env0
  , normalizationEnv = Norm.env0
  }

-- keep history in opposite order from HL standard (newest at end of file)

haskelineSettings :: HL.Settings IO
haskelineSettings = HL.defaultSettings {HL.autoAddHistory = False}

revHistory :: HL.History -> HL.History
revHistory = foldl (flip HL.addHistory) HL.emptyHistory . HL.historyLines

writeHistory :: Conf -> HL.History -> IO ()
writeHistory Conf{funFile} = HL.writeHistory funFile . revHistory

readHistory :: Conf -> IO HL.History
readHistory Conf{funFile} = fmap revHistory $ HL.readHistory funFile

-- replay .history lines
replay :: Conf -> Env -> [String] -> IO Env
replay conf env = \case
  [] -> return env
  line:earlier -> do
    env1 <- replay conf env earlier
    pep conf putStrLn line env1 >>= \case
      Nothing -> return env1
      Just env2 -> return env2

-- read-eval-print-loop
repl :: Conf -> Int -> Env -> HL.InputT IO ()
repl conf n env = do
  HL.getInputLine (col AN.Magenta $ show n <> "> ") >>= \case
    Nothing -> return ()
    Just line -> do
      HL.modifyHistory (HL.addHistory line)
      HL.getHistory >>= lift . writeHistory conf
      let noput _ = return ()
      lift (pep conf noput line env) >>= \case
        Nothing -> repl conf n env
        Just env' -> repl conf (n + 1) env'


slowCol,fastCol,normCol :: AN.Color
slowCol = AN.Blue
fastCol = AN.Cyan
normCol = AN.Green

-- parse-eval-print
pep :: Conf -> (String -> IO ()) -> String -> Env -> IO (Maybe Env)
pep Conf{verbose,doNorm} put line env@Env{evaluationEnvS,evaluationEnvF,normalizationEnv} = do

  let showSlow = not doNorm || verbose

  case parseDef line of

    Left s -> do
      putStrLn $ col AN.Red $ "parse error: " <> s <> " : " <> line
      return Nothing

    Right Nothing -> do
      return Nothing

    Right (Just (Left (Def name exp))) -> do
      put line
      --when verbose $ putStrLn $ "ORIG-> " <> (col AN.Magenta $ show exp)
      evaluationEnvS <- define showSlow slowCol name exp evaluationEnvS

      if doNorm then
        Norm.normalize normalizationEnv exp >>= \case
          Left s -> do
            putStrLn $ col AN.Red $ "error during normalization: " <> s
            return Nothing
          Right ((sem,expF),Norm.Counts{Norm.beta}) -> do
            if beta>0 then putStrLn $ col normCol $ "(beta:" <> show beta <> ")" else return ()
            when verbose $ putStrLn $ "NORM-> " <> (col AN.Green $ show expF)
            evaluationEnvF <- define verbose fastCol name expF evaluationEnvF
            return $ Just $ env
              { evaluationEnvS
              , evaluationEnvF
              , normalizationEnv = Map.insert name (return sem) normalizationEnv
              }
        else
        return $ Just $ env { evaluationEnvS }


    Right (Just (Right exp)) -> do
      put line
      (value1,counts1) <- Eval.run evaluationEnvS (Eval.eval exp)
      when showSlow $ printValue (slowCol,slowCol) counts1 value1

      when doNorm $ do
        (value2,counts2) <- Eval.run evaluationEnvF (Eval.eval exp)
        let col1 = (if exceptFun value2 /= exceptFun value1 then AN.Red else fastCol)
        let col2 = (if countsWorsen counts1 counts2 then AN.Red else fastCol)
        printValue (col1,col2) counts2 value2

      return ()

{-
      when verbose $ putStrLn $ "ORIG-> " <> (col AN.Magenta $ show exp)
      Norm.normalize normalizationEnv exp >>= \case
        Left s -> do
          putStrLn $ col AN.Red $ "error during normalization: " <> s
          return ()
        Right (exp',Norm.Counts{Norm.beta}) -> do
          if beta>0 then putStrLn $ col AN.Cyan $ "(beta:" <> show beta <> ")" else return ()
          when verbose $ putStrLn $ "NORM-> " <> (col AN.Green $ show exp')
-}
{-
      when normCheck $
        case exceptFun value2 of
          Nothing -> return ()
          Just e1 -> do
            n <- Norm.normalize normalizationEnv exp
            let mE2 =
                  case n of
                    Left s -> Just (Left s)
                    Right ((_,EBase base),_) -> Just (Right base)
                    Right (_,_) -> Nothing
            when (mE2 /= Just e1) $
              putStrLn $ col AN.Red $ "base norm failed : " -- <> show n
            return ()
-}
      return Nothing


exceptFun :: Value -> Maybe (Either String Base)
exceptFun = \case
  VError s -> Just (Left s)
  VBase b -> Just (Right b)
  VFun{} -> Nothing


define :: Bool -> AN.Color -> String -> Exp -> EE -> IO EE
define display c1 name exp env = do
  (value,counts) <- Eval.run env (Eval.eval exp)
  when display $ printValue (c1,c1) counts value
  return $ Map.insert name (return value) env

printValue :: (AN.Color,AN.Color) -> Eval.Counts -> Value -> IO ()
printValue (c1,c2) counts = \case
  VError s  -> putStrLn $ col AN.Red $ "eval error: " <> s <> show counts
  v -> putStrLn $ col c1 (show v) <> col c2 (show counts)


col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]
