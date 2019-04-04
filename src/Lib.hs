{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Data.Functor
import Control.Monad
import Control.Monad.Except
import Control.Applicative

import Text.Trifecta
import Text.Parser.Combinators


data Term
    = Atom String
    | Number Integer
    | String String
    | Boolean Bool
    | List [Term]
    deriving Eq

instance Show Term where
    show (Atom str) = str
    show (Number n) = show n
    show (String str) = show str
    show (Boolean bool) = show bool
    show (List xs) = "(" ++ unwords (show <$> xs) ++ ")"

data EvalError = TypeError String | TooManyArguments deriving Eq

instance Show EvalError where
    show (TypeError xs) = "TypeError: " ++ xs

----------------
---- Parser ----
----------------

parseNumber :: Parser Term
parseNumber = Number <$> integer

parseAtom :: Parser Term
parseAtom = do
    x  <- letter
    xs <- many (alphaNum <|> char '?')
    return $ Atom (x:xs)

parseString' :: Parser Term
parseString' = String <$> between (char '"') (char '"') (some alphaNum)

parseBool :: Parser Term
parseBool = Boolean <$> (string "True" $> True <|> (string "False" $> False))

parseScalars :: Parser Term
parseScalars = parseNumber <|> parseString' <|> parseBool <|> parseAtom 

parseRegList :: Parser Term
parseRegList = parens $ List . foldr (:) [] <$> parseTerm `sepBy` spaces
 
parseDotList :: Parser Term
parseDotList = parens $ List . foldr (:) [] <$> parseTerm `sepBy` token (char '.')

parseList :: Parser Term
parseList = try parseDotList <|> try parseRegList

parseTerm :: Parser Term
parseTerm = parseScalars <|> parseList

parse :: String -> Result Term
parse = parseString parseTerm mempty 


------------------
--- Evaluation ---
------------------
-- | Primitive Functions to be implented:
-- eq?
-- quote
-- cons
-- car
-- cdr
-- atom?
-- define
-- lambda
-- cond

evalTerm :: MonadError EvalError m => Term -> m Term
evalTerm (List (Atom "add": args)) = Number . sum <$> traverse (asInteger <=< evalTerm) args
evalTerm (List (Atom "eq?": args)) = Boolean . equal <$> traverse evalTerm args
evalTerm (List xs) = List <$> traverse evalTerm xs
evalTerm expr = return expr

equal :: Eq a => [a] -> Bool
equal (x:xs) = all (x ==) xs

-- equal :: MonadError EvalError m => [Term] -> m Bool
-- equal [x, y] = return $ x == y
-- equal _ = throwError TooManyArguments

asInteger :: MonadError EvalError m => Term -> m Integer
asInteger (Number n) = return n
asInteger term = throwError . TypeError $ "'" ++ show term ++ "'" ++ " is not an Integer"

execEval :: String -> Result (Either EvalError Term)
execEval str = runExcept . evalTerm <$> parse str
