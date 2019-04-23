{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import qualified Data.ByteString as BS
import Data.Text (Text(..), pack, unpack)
import Data.Text.Encoding
import Data.Text.Encoding.Error

import System.Console.Haskeline (runInputT, defaultSettings)
import System.Environment
import System.IO

readFilePath :: Text -> IO Text
readFilePath = fmap (decodeUtf8With lenientDecode) . BS.readFile . unpack

readArgs :: IO [Text]
readArgs = (fmap . fmap) pack getArgs

main :: IO ()
main = do
  [mode, arg] <- readArgs
  case mode of
    "-o" -> do
      contents <- readFilePath arg
      let env = Env contents Normal
      result <- runAppM env interpret
      print result
    "-e" -> do
      let env = Env arg Normal
      result <- runAppM env interpret
      print result
    "-i" -> undefined 
    _ -> undefined
