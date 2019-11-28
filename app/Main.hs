
module Main (main) where

import Control.Monad.Trans.Class (lift)
import Eval (Def(..),Value(..),Eff)
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
  }

defaultConf :: Conf
defaultConf = Conf
  { funFile = ".history"
  }

parseArgs :: [String] -> Conf
parseArgs args = loop args defaultConf
  where
    loop args conf = case args of
      [] -> conf
      funFile:rest -> loop rest $ conf { funFile }

start :: Conf -> HL.InputT IO ()
start conf = do
  history <- lift $ readHistory conf
  HL.putHistory history
  env <- lift $ replay env0 (HL.historyLines history)
  repl conf 1 env


data Env = Env
  { evaluationEnv :: EE
  , evaluationEnv2 :: EE
  , normalizationEnv  :: Map String (Norm.Eff Norm.Sem)
  }

type EE = Map String (Eval.Eff Eval.Value)

env0 :: Env
env0 = Env
  { evaluationEnv = Eval.env0
  , evaluationEnv2 = Eval.env0
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
replay :: Env -> [String] -> IO Env
replay env = \case
  [] -> return env
  line:earlier -> do
    env1 <- replay env earlier
    putStr line
    ep line env1 >>= \case
      Nothing -> return env1
      Just env2 -> return env2

-- read-eval-print-loop
repl :: Conf -> Int -> Env -> HL.InputT IO ()
repl conf n env = do
  HL.getInputLine (col AN.Green $ show n <> "> ") >>= \case
    Nothing -> return ()
    Just line -> do
      HL.modifyHistory (HL.addHistory line)
      HL.getHistory >>= lift . writeHistory conf
      lift (ep line env) >>= \case
        Nothing -> repl conf n env
        Just env' -> repl conf (n + 1) env'

-- eval-print
ep :: String -> Env -> IO (Maybe Env)
ep line env =
  if line == ""
  then return $ Just env
  else parseEval line env


parseEval :: String -> Env -> IO (Maybe Env)
parseEval line env@Env{evaluationEnv,evaluationEnv2,normalizationEnv} = do
  case parseDef line of
    Left s -> do
      putStrLn $ col AN.Red $ "parse error: " <> s <> " : " <> line
      return Nothing
    Right (Left (Def name exp)) -> do
      Norm.normalize normalizationEnv exp >>= \case
        Left s -> do
          putStrLn $ col AN.Red $ "error during normalization: " <> s
          return Nothing
        Right (exp',Norm.Counts{Norm.beta}) -> do
          putStrLn $ (if beta>0 then col AN.Blue $ " (beta:" <> show beta <> ")" else "")
          --putStrLn $ "NORM-> " <> (col AN.Green $ show exp')
          return $ Just $ env
            { evaluationEnv = Map.insert name (Eval.eval exp) evaluationEnv
            , evaluationEnv2 = Map.insert name (Eval.eval exp') evaluationEnv2
            , normalizationEnv = Map.insert name (Norm.norm exp) normalizationEnv
            }
    Right (Right exp) -> do
      putStrLn ""
      showEval AN.Cyan evaluationEnv exp
      showEval AN.Blue evaluationEnv2 exp
      return Nothing

showEval :: AN.Color -> EE -> Eval.Exp -> IO ()
showEval colour ee exp = do
  (value,counts) <- Eval.run ee (Eval.eval exp)
  case value of
    VError s -> putStrLn $ col AN.Red $ "eval error: " <> s <> show counts
    v -> putStrLn $ col colour $ show v <> show counts


col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]
