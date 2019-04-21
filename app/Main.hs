module Main where

import Lib
import System.Console.Haskeline (runInputT, defaultSettings)

main :: IO ()
main = print () -- runInputT defaultSettings repl
