
module Main (main) where

import System.Environment (getArgs)
import Control.Monad.Trans.Class (lift)
import Eval (Def(..),Exp(..),Value(..),evaluate,primInt2String)
import Norm (Env,normalize)
import Parse (parseDef)
import qualified Eval (Env,env0,extend)
import qualified Norm (env0,define,preDefined)
import qualified System.Console.ANSI as AN
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.History as HL

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
  env <- lift $ replay initialNormEnv (HL.historyLines history)
  repl conf 1 env

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
parseEval line env = do
  case parseDef line of
    Left s -> do
      putStrLn $ col AN.Red $ "parse error: " <> s <> " : " <> line
      return Nothing

    Right (Left (Def name exp)) -> do
      --putStrLn $ "ORIG:" <> show exp
      Norm.normalize env exp >>= \case
        Left s -> do
          putStrLn $ col AN.Red $ "error during normalization: " <> s
          return Nothing
        Right (exp',_inlineCount) -> do
          --putStrLn $ "NORM-> " <> (col AN.Green $ show exp')
          putStrLn $ col AN.Green $ " " <> show _inlineCount
          return $ Just $ Norm.define name exp' env

    Right (Right exp) -> do
      Norm.normalize env exp >>= \case
        Left s -> do
          putStrLn $ col AN.Red $ "error during normalization: " <> s
          return Nothing
        Right (exp',_inlineCount) -> do
          --putStrLn $ "NORM-> " <> (col AN.Green $ show exp')
          putStr $ col AN.Green $ " " <> show _inlineCount <> " "
          (value,counts) <- evaluate initialEvalEnv exp'
          case value of
            VError s -> do
              putStrLn $ col AN.Red $ "eval error: " <> s <> show counts
              return Nothing
            v -> do
              putStrLn $ col AN.Cyan $ show v <> show counts
              return Nothing


initialNormEnv :: Env
initialNormEnv = foldr Norm.preDefined Norm.env0 ["noinline","primInt2String"]

initialEvalEnv :: Eval.Env
initialEvalEnv = foldr (uncurry Eval.extend) Eval.env0
  [ ("noinline", return $ VFun (EVar "_noinline_") $ \eff -> eff)
  , ("primInt2String", return $ primInt2String)
  ]

col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]
