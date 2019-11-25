
module Main (main) where

import Control.Monad.Trans.Class (lift)
import Eval (Def(..),Value(VFun,VError),evaluate)
import Norm (Env,normalize)
import Parse (parseDef)
import qualified Eval (Env,env0,extend)
import qualified Norm (env0,define,preDefined)
import qualified System.Console.ANSI as AN
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.History as HL

main :: IO ()
main = HL.runInputT haskelineSettings $ start

start :: HL.InputT IO ()
start = do
    history <- lift $ readHistory
    HL.putHistory history
    env <- lift $ replay initialNormEnv (HL.historyLines history)
    repl 1 env

-- keep history in opposite order from HL standard (newest at end of file)

haskelineSettings :: HL.Settings IO
haskelineSettings = HL.defaultSettings {HL.autoAddHistory = False}

revHistory :: HL.History -> HL.History
revHistory = foldl (flip HL.addHistory) HL.emptyHistory . HL.historyLines

writeHistory :: HL.History -> IO ()
writeHistory = HL.writeHistory ".history" . revHistory

readHistory :: IO HL.History
readHistory = fmap revHistory $ HL.readHistory ".history"

-- replay .history lines
replay :: Env -> [String] -> IO Env
replay env =
    \case
        [] -> return env
        line:earlier -> do
            env1 <- replay env earlier
            putStrLn line
            ep line env1 >>= \case
                Nothing -> return env1
                Just env2 -> return env2

-- read-eval-print-loop
repl :: Int -> Env -> HL.InputT IO ()
repl n env = do
    HL.getInputLine (col AN.Green $ show n <> "> ") >>= \case
        Nothing -> return ()
        Just line -> do
            HL.modifyHistory (HL.addHistory line)
            HL.getHistory >>= lift . writeHistory
            lift (ep line env) >>= \case
                Nothing -> repl n env
                Just env' -> repl (n + 1) env'

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
        Right exp' -> do
          putStrLn $ "NORM-> " <> (col AN.Green $ show exp')
          return $ Just $ Norm.define name exp' env

    Right (Right exp) -> do
      Norm.normalize env exp >>= \case
        Left s -> do
          putStrLn $ col AN.Red $ "error during normalization: " <> s
          return Nothing
        Right exp' -> do
          putStrLn $ "NORM-> " <> (col AN.Green $ show exp')
          (value,counts) <- evaluate initialEvalEnv exp'
          case value of
            VError s -> do
              putStrLn $ col AN.Red $ "eval error: " <> s <> show counts
              return Nothing
            v -> do
              putStrLn $ col AN.Cyan $ show v <> show counts
              return Nothing

initialNormEnv :: Env
initialNormEnv = foldr Norm.preDefined Norm.env0 ["noinline"]

initialEvalEnv :: Eval.Env
initialEvalEnv = foldr (uncurry Eval.extend) Eval.env0 [
  ("noinline", return $ VFun $ \eff -> eff)
  ]


col :: AN.Color -> String -> String
col c s =
    AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
    AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]
