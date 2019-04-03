module Lib where

import Data.Functor
import Control.Monad
import Control.Applicative

import Text.Trifecta
import Text.Parser.Combinators


data Term
    = Atom String
    | Number Integer
    | String String
    | Boolean Bool
    | Cons Term Term
    | Nil
    deriving (Show, Eq)


----------------
---- Parser ----
----------------

parseNumber :: Parser Term
parseNumber = Number <$> integer

parseAtom :: Parser Term
parseAtom = do
    x  <- letter
    xs <- many alphaNum
    return $ Atom (x:xs)

parseString' :: Parser Term
parseString' = String <$> between (char '"') (char '"') (some alphaNum)

parseBool :: Parser Term
parseBool = Boolean <$> (string "True" $> True <|> (string "False" $> False))

parseScalars :: Parser Term
parseScalars = parseNumber <|> parseString' <|> parseBool <|> parseAtom 

parseRegList :: Parser Term
parseRegList = parens $ foldr Cons Nil <$> parseTerm `sepBy` spaces
 
parseDotList :: Parser Term
parseDotList = parens $ foldr Cons Nil <$> parseTerm `sepBy` token (char '.')

parseList :: Parser Term
parseList = try parseDotList <|> try parseRegList

parseTerm :: Parser Term
parseTerm = parseScalars <|> parseList

parse :: String -> Result Term
parse = parseString parseTerm mempty 


------------------
--- Evaluation ---
------------------
-- | I'm not happy with any of this evaluation. I think I need to convert from 

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

evalTerm :: Term -> Term
evalTerm (Cons (Atom func) args) = apply func $ evalTerm <$> toList args
evalTerm expr = expr

apply :: String -> [Term] -> Term
apply func args = maybe (Boolean False) ($ args) $ lookup func primitives

primitives :: [(String, [Term] -> Term)]
primitives = [ ("add", \xs -> Number . sum $ unpackNum <$> xs)
             , ("eq?", undefined)
             , ("quote", undefined)
             , ("cons", undefined)
             , ("car", undefined)
             , ("cdr", undefined)
             , ("atom?", undefined)
             , ("defined", undefined)
             , ("lambda", undefined)
             , ("cond", undefined)
             ]

unpackNum :: Term -> Integer
unpackNum (Number n) = n
unpackNum _ = 0
    
-- unsafe!
toList :: Term -> [Term]
toList (Cons x xs) = x : toList xs
toList Nil = []
toList _ = undefined
