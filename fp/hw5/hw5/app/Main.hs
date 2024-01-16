module Main
  ( main,
  )
where

import Control.Monad.IO.Class
import Data.Set
import HW5.Action
import HW5.Base
import HW5.Evaluator
import HW5.Parser
import HW5.Pretty
import Prettyprinter.Render.Terminal (putDoc)
import System.Console.Haskeline
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      line <- getInputLine "hi> "
      case line of
        Nothing -> return ()
        Just "exit" -> return ()
        Just input -> do
          handleInput input
          loop

    handleInput :: String -> InputT IO ()
    handleInput input =
      case parse input of
        Left err -> liftIO $ print $ errorBundlePretty err
        Right expr -> handleResult =<< liftIO (runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime]))

    handleResult :: Either HiError HiValue -> InputT IO ()
    handleResult result =
      case result of
        Left err -> liftIO $ print err
        Right val -> liftIO $ do
          putDoc $ prettyValue val
          putStrLn ""