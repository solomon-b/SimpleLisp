{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Control.Monad

import Data.Functor
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Text.Trifecta

import Evaluator.Types (Term(..), DotList(..), listToDot)

data ParseError = ParseError deriving Show

-- | TODO: Investigate possibility custom error reporting in Trifecta
-- | ()) should fail

parseNumber :: Parser Term
parseNumber = Number <$> integer

parseSymbol :: Parser Term
parseSymbol = do
  x <- letter <|> oneOf "?+*/%"
  xs <- many (alphaNum <|> oneOf "?+*/%")
  return $ Symbol (x:xs)
--parseSymbol = (\x xs -> Symbol (x:xs)) <$> letter <*> many (alphaNum <|> char '?')

parseString' :: Parser Term
parseString' = String <$> between (char '"') (char '"') (some alphaNum)

parseBool :: Parser Term
parseBool = Boolean <$> (string "True" $> True <|> (string "False" $> False))

parseQuote :: Parser Term
parseQuote = (\x -> List (Symbol "quote" :-: x :-. Nil)) <$> (void (char '\'') *> parseTerm)

parseScalars :: Parser Term
parseScalars = parseQuote <|> parseBool <|> parseSymbol <|> parseNumber <|> parseString' 

parseRegList :: Parser Term
parseRegList = parens $ do
  xs <- parseTerm `sepBy` spaces
  if null xs
  then return Nil
  else return . List $ f xs
  where f [] = undefined
        f [x] = x :-. Nil
        f (x:xs) = x :-: f xs

parseDotList :: Parser Term
parseDotList = parens $ do
  ts <- parseTerm `sepEndBy` spaces
  void $ token (char '.')
  t <- parseTerm
  return . DotList $ listToDot $ ts ++ [t]

parseList :: Parser Term
parseList = try parseDotList <|> try parseRegList

parseTerm :: Parser Term
parseTerm = parseScalars <|> parseList

parse :: Text -> Result Term
parse = parseByteString parseTerm mempty . encodeUtf8
