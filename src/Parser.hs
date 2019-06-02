{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Control.Monad

import Data.Functor
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Text.Trifecta

import Evaluator.Types (Term(..))

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
parseQuote = (\x -> List [Symbol "quote", x]) <$> (void (char '\'') *> parseTerm)

parseScalars :: Parser Term
parseScalars = try parseLambda
  <|> try parseQuote
  <|> try parseBool
  <|> try parseSymbol
  <|> try parseNumber
  <|> try parseString' 

parseRegList :: Parser Term
parseRegList = parens $  List <$> parseTerm `sepBy` spaces

parseDotList :: Parser Term
parseDotList = parens $ do
  ts <- parseTerm `sepEndBy` spaces
  void $ token (char '.')
  t <- parseTerm
  return . DotList $ (ts, t)

parseList :: Parser Term
parseList = try parseDotList <|> try parseRegList

parseParams :: Parser [Term]
parseParams = parens $ (fmap . fmap) Symbol $ some letter `sepEndBy` spaces

parseLambda :: Parser Term
parseLambda = token . parens $ do
  whiteSpace
  void . token $ string "lambda"
  params <- parseParams
  whiteSpace
  body <- parseList
  return $ Func params body

parseTerm :: Parser Term
parseTerm = try parseScalars <|> try parseList 

parse :: Text -> Result Term
parse = parseByteString parseTerm mempty . encodeUtf8
