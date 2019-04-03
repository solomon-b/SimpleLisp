module Lib where

import Data.Functor
import Control.Monad
import Control.Applicative

import Text.Trifecta
import Text.Parser.Combinators


data Expr
    = Atom String
    | Number Integer
    | String String
    | Boolean Bool
    | Cons Expr Expr
    | Nil
    deriving (Show, Eq)

parseNumber :: Parser Expr
parseNumber = Number <$> integer

parseAtom :: Parser Expr
parseAtom = do
    x  <- letter
    xs <- many alphaNum
    return $ Atom (x:xs)

parseString' :: Parser Expr
parseString' = String <$> between (char '"') (char '"') (some alphaNum)

parseBool :: Parser Expr
parseBool = Boolean <$> (string "True" $> True <|> (string "False" $> False))

parseScalars :: Parser Expr
parseScalars = parseNumber <|> parseString' <|> parseBool <|> parseAtom 

parseRegList :: Parser Expr
parseRegList = parens $ foldr Cons Nil <$> parseExpr `sepBy` spaces
 
parseDotList :: Parser Expr
parseDotList = parens $ foldr Cons Nil <$> parseExpr `sepBy` token (char '.')

parseList :: Parser Expr
parseList = try parseDotList <|> try parseRegList

parseExpr :: Parser Expr
parseExpr = parseScalars <|> parseList

evalExpr :: Expr -> Expr
evalExpr (Cons (Atom func) args) = apply func $ evalExpr <$> toList args
evalExpr expr = expr

apply :: String -> [Expr] -> Expr
apply func args = maybe (Boolean False) ($ args) $ lookup func primitives

primitives :: [(String, [Expr] -> Expr)]
primitives = [("add", \xs -> Number . sum $ unpackNum <$> xs)]

unpackNum :: Expr -> Integer
unpackNum (Number n) = n
unpackNum _ = 0
    
-- unsafe!
toList :: Expr -> [Expr]
toList (Cons x xs) = x : toList xs
toList Nil = []
toList _ = undefined

parse :: String -> Result Expr
parse = parseString parseExpr mempty 
