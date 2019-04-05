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
    | DottedList [Term]
    deriving (Show, Eq)

--instance Show Term where
--    show (Atom str) = str
--    show (Number n) = show n
--    show (String str) = show str
--    show (Boolean bool) = show bool
--    show (List xs) = "(" ++ unwords (show <$> xs) ++ ")"

-- | TODO: Add more Error types
data EvalError = TypeError String | TooManyArguments deriving Eq

-- | TODO: Improve Error Show instances
instance Show EvalError where
    show (TypeError xs) = "TypeError: " ++ xs
    show TooManyArguments = "Too Many Arguments"


----------------
---- Parser ----
----------------

parseNumber :: Parser Term
parseNumber = Number <$> integer

parseAtom :: Parser Term
parseAtom = (\x xs -> Atom (x:xs)) <$> letter <*> many (alphaNum <|> char '?')

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
-- | Primitive Functions:
-- eq?
-- quote
-- cons
-- car
-- cdr
-- atom?
-- define
-- lambda
-- cond

-- | Factor out the primitive function pattern matching into a global context object (ReaderT)
evalTerm :: MonadError EvalError m => Term -> m Term
evalTerm (List (Atom "car"   : args)) = car                  =<< traverse evalTerm args
evalTerm (List (Atom "atom?" : args)) = atom                 =<< traverse evalTerm args
evalTerm (List (Atom "eq?"   : args)) = fmap Boolean . equal =<< traverse evalTerm args
evalTerm (List (Atom "cdr"   : args)) = fmap List    . cdr   =<< traverse evalTerm args
evalTerm (List (Atom "add"   : args)) = Number . sum         <$> traverse (asInteger <=< evalTerm) args
evalTerm (List xs)                    = List                 <$> traverse evalTerm xs
evalTerm expr = return expr

asInteger :: MonadError EvalError m => Term -> m Integer
asInteger (Number n) = return n
asInteger term = throwError . TypeError $ "'" ++ show term ++ "'" ++ " is not an Integer."

equal :: MonadError EvalError m => [Term] -> m Bool
equal [x, y] = return $ x == y
equal _ = throwError TooManyArguments

-- | TODO: Can I get rid of the case statement?
car :: MonadError EvalError m => [Term] -> m Term
car [] = throwError . TypeError $ "The object () passed to car is not the right type."
car (x:xs) =
    case x of
        List (x:xs) -> return x
        List []     -> throwError . TypeError $ "The object () passed to car is not the right type."
        term        -> throwError . TypeError $ "The object " ++ show term ++ " passed to car is not the right type."
       
cdr :: MonadError EvalError m => [Term] -> m [Term]
cdr [] = throwError . TypeError $ "The object () passed to cdr is not the right type."
cdr (x:xs) =
    case x of
        List (x:xs) -> return xs
        List []     -> throwError . TypeError $ "The object () passed to cdr is not the right type."
        term        -> throwError . TypeError $ "The object " ++ show term ++ " passed to cdr is not the right type."

-- | TODO:
-- | should (atom? (x))) == True ?
-- | should this only be true for bound atoms?
atom :: MonadError EvalError m => [Term] -> m Term
atom [Atom _] = return $ Boolean True
atom [_]      = return $ Boolean False
atom a@(x:xs) = throwError . TypeError $ "The object " ++ show a ++ " passed to atom? is not the right type."

execEval :: String -> Result (Either EvalError Term)
execEval str = runExcept . evalTerm <$> parse str
