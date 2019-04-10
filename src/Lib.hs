{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Data.Typeable
import Data.Functor
import Data.List (intercalate)

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Applicative

import System.IO
import System.Console.Haskeline

import Text.Trifecta
import Text.Parser.Combinators

data Term
  = Symbol String
  | Number Integer
  | String String
  | Boolean Bool
  | List [Term]
  | DotList [Term] Term
  deriving (Eq)

instance Show Term where
    show (Symbol str) = str
    show (Number n) = show n
    show (String str) = show str
    show (Boolean bool) = show bool
    show (List xs) = "(" ++ unwords (show <$> xs) ++ ")"
    show (DotList xs x) = "(" ++ unwords (show <$> xs) ++ " . " ++ show x ++ show ")"

-- | TODO: Add more Error types
data EvalError
  = TypeError String Term
  | TooManyArguments String Int
  | ObjectNotApplicable Term
  deriving Eq

-- | TODO: Improve Error Show instances
instance Show EvalError where
  show (TypeError func term) = "TypeError: The object " ++ show term ++ " passed to " ++ func ++ " is not the right type."
  show (TooManyArguments func args) = "The procedure "         ++ func      ++
                                      " has been called with " ++ show args ++
                                      " arguments; it requires exactly 1 argument."
  show (ObjectNotApplicable obj) = "The object '" ++ show obj ++ "' is not applicable."


data Expr where
  Numby :: Integer -> Expr
  Truthy :: Bool -> Expr 
  Stringy :: String -> Expr

elimExpr :: (forall a. a -> r) -> Expr -> r
elimExpr f (Numby a) = f a
elimExpr f (Truthy a) = f a

data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

----------------
---- Parser ----
----------------
-- | TODO: Investigate possibility custom error reporting in Trifecta

parseNumber :: Parser Term
parseNumber = Number <$> integer

parseSymbol :: Parser Term
parseSymbol = (\x xs -> Symbol (x:xs)) <$> letter <*> many (alphaNum <|> char '?')

parseString' :: Parser Term
parseString' = String <$> between (char '"') (char '"') (some alphaNum)

parseBool :: Parser Term
parseBool = Boolean <$> (string "True" $> True <|> (string "False" $> False))

parseQuote :: Parser Term
parseQuote = (\x -> List [Symbol "quote", x]) <$> (void (char '\'') *> parseTerm)

parseScalars :: Parser Term
parseScalars = parseQuote <|> parseNumber <|> parseString' <|> parseBool <|> parseSymbol 

parseRegList :: Parser Term
parseRegList = parens $ List . foldr (:) [] <$> parseTerm `sepBy` spaces
 
parseDotList :: Parser Term
parseDotList = parens $ do
  car <- foldr (:) [] <$> parseTerm `sepBy` spaces
  void . token $ char '.'
  cdr <- parseTerm
  return $ DotList car cdr

parseList :: Parser Term
parseList = try parseDotList <|> try parseRegList

parseTerm :: Parser Term
parseTerm = parseScalars <|> parseList

parse :: String -> Result Term
parse = parseString parseTerm mempty 


------------------
--- Evaluation ---
------------------
-- | TODO: Primitive Functions
-- define
-- lambda
-- cond

-- | Factor out the primitive function pattern matching into a global context object (ReaderT)
evalTerm :: MonadError EvalError m => Term -> m Term
evalTerm (List [Symbol "quote", value]) = return value
evalTerm (List (Symbol "cons"  : args)) = cons                 =<< traverse evalTerm args
evalTerm (List (Symbol "car"   : args)) = car                  =<< traverse evalTerm args
evalTerm (List (Symbol "atom?" : args)) = atom                 =<< traverse evalTerm args
evalTerm (List (Symbol "eq?"   : args)) = fmap Boolean . equal =<< traverse evalTerm args
evalTerm (List (Symbol "cdr"   : args)) = fmap List    . cdr   =<< traverse evalTerm args
evalTerm (List (Symbol "add"   : args)) = Number . sum         <$> traverse (asInteger <=< evalTerm) args
evalTerm (List [])                     = List                  <$> traverse evalTerm []
evalTerm (List xs)                     = badAppplication       =<< traverse evalTerm xs
evalTerm expr = return expr

asInteger :: MonadError EvalError m => Term -> m Integer
asInteger (Number n) = return n
asInteger term = throwError $ TypeError "asInteger" term


badAppplication :: MonadError EvalError m => [Term] -> m Term
badAppplication (x:xs) = throwError $ ObjectNotApplicable x

equal :: MonadError EvalError m => [Term] -> m Bool
equal [x, y] = return $ x == y
equal xs = throwError $ TooManyArguments "equal" (length xs)

cons :: MonadError EvalError m => [Term] -> m Term
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DotList xs last] = return $ DotList (x:xs) last
cons [x, y] = return $ DotList [x] y
cons terms = throwError $ TooManyArguments (show terms) (length terms)

car :: MonadError EvalError m => [Term] -> m Term
car [] = throwError $ TypeError "car" (List [])
car (x:xs) =
  case x of
    List (x:xs) -> return x
    List []     -> throwError $ TypeError "car" (List [])
    term        -> throwError $ TypeError "car" term
       
cdr :: MonadError EvalError m => [Term] -> m [Term]
cdr [] = throwError $ TypeError "cdr" (List [])
cdr (x:xs) =
  case x of
    List (x:xs) -> return xs
    List []     -> throwError $ TypeError "cdr" (List [])
    term        -> throwError $ TypeError "cdr" term

atom :: MonadError EvalError m => [Term] -> m Term
atom xs
  | length xs > 1 = throwError $ TooManyArguments "atom?" (length xs)
  | null xs = return $ Boolean False
  | otherwise =
    case xs of
      [List _] -> return $ Boolean False
      [DotList _ _] -> return $ Boolean False
      _ -> return $ Boolean True

execEval :: String -> Result (Either EvalError Term)
execEval str = runExcept . evalTerm <$> parse str


--------------
---- REPL ----
--------------

type Repl a = InputT IO a

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

repl :: Repl ()
repl = forever $ do
  rawInput <- getInputLine "> "
  case rawInput of
    Nothing -> repl
    Just input -> do
       let parsedResult = parse input
       let evalResult = execEval input
       liftIO $ print evalResult
