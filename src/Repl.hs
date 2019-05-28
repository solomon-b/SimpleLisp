module Repl where

import Data.Text (pack)
import System.Console.Haskeline

import Control.Monad.IO.Class

import Evaluator
import Evaluator.Types
import Parser

printResult :: (Show a, Show b) => Either a b -> IO ()
printResult = either print print

repl :: EvalEnv -> IO ()
repl initialEnv = runInputT defaultSettings (loop initialEnv)
  where loop env = do
          mstr <- getInputLine "> "
          case mstr of
            Just str -> do
              let (res, env') = runLispM env . eval . parse $ pack str
              liftIO $ printResult res
              --liftIO $ print env'
              loop env'
            Nothing -> loop env
