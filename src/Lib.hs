{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Debug.Trace

import Data.Typeable
import Data.Functor
import Data.List (intercalate)

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Applicative

import GHC.Exts (IsList(..))
import System.IO
import System.Console.Haskeline

import Text.Trifecta
import Text.Parser.Combinators

data DotList a = Nil | a :-: (DotList a) | a :-. a
  deriving (Functor, Foldable, Traversable, Eq, Show)

infixr 5 :-:
infixr 5 :-.

--instance Show a => Show (DotList a) where
--  show (v :-: Nil) = show v
--  show (v :-: vs)  = show v <> " " <> show vs
--  show (a :-. b)   =  show a <> " . " <> show b
--  show Nil         = ""

instance IsList (DotList a) where
  type Item (DotList a) = a
  fromList = foldr (:-:) Nil
  toList Nil = []
  toList (t :-: ts) = t : toList ts

listToDot :: [a] -> DotList a
listToDot [t, t'] = t :-. t'
listToDot (t:ts) = t :-: listToDot ts

data Term
  = Symbol String
  | Number Integer
  | String String
  | Boolean Bool
  | List (DotList Term)
  | DotList (DotList Term)
  deriving (Eq, Show)

data Arrity = Unary Term | Binary Term Term | Nary (DotList Term)

--instance Semigroup Term where
--  (<>) (List ts) (List ts') = List (ts <> ts')
--  (<>) term term' = DotList [term] term'

--instance Monoid Term where
--  mempty = Nil
--  mappend = (<>)

--instance Show Term where
--    show (Symbol str) = str
--    show (Number n) = show n
--    show (String str) = show str
--    show (Boolean bool) = show bool
--    show (List xs) = "(" ++ show xs ++ ")"
--    show (DotList xs) = "(" ++ show xs ++ ")"

data EvalError
  = TypeError String Term
  | WrongArrity String Int Int
  | ObjectNotApplicable Term
  | UnspecifiedReturn
  | IllFormedSyntax
  deriving Eq

-- | TODO: Improve Error Show instances
instance Show EvalError where
  show (TypeError func term) = "TypeError: The object " ++ show term ++ " passed to " ++ func ++ " is not the right type."
  show (WrongArrity func arrity params) = "The procedure " ++ func ++ " has been called with " ++ show params ++
                                      " arguments; it requires exactly " ++ show arrity ++ " argument"
  show (ObjectNotApplicable obj) = "The object '" ++ show obj ++ "' is not applicable."
  show UnspecifiedReturn = "Unspecified return value."
  show IllFormedSyntax = "Ill-formed syntax"


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
parseQuote = (\x -> List (Symbol "quote" :-: x :-: Nil)) <$> (void (char '\'') *> parseTerm)

parseScalars :: Parser Term
parseScalars = parseQuote <|> parseNumber <|> parseString' <|> parseBool <|> parseSymbol 

parseRegList :: Parser Term
parseRegList = parens $ List . foldr (:-:) Nil <$> parseTerm `sepBy` spaces

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

parse :: String -> Result Term
parse = parseString parseTerm mempty 

------------------
--- Evaluation ---
------------------
-- | TODO: Primitive Functions
-- atom?  ✓ 
-- eq     ✓
-- car    ✓
-- cdr    ✓
-- cons   ✓
-- quote  ✓
-- cond   ✓
-- lambda
-- label
-- fix :D

-- | Factor out the primitive function pattern matching into a global context object (ReaderT)
evalTerm :: MonadError EvalError m => Term -> m Term
evalTerm (List (Symbol "quote" :-: value :-: Nil)) = return value
evalTerm (List (Symbol "atom?" :-: args)) = arrity 1 "atom?" atom =<< traverse evalTerm args
evalTerm (List (Symbol "cons"  :-: args)) = arrity 2 "cons" cons  =<< traverse evalTerm args
evalTerm (List (Symbol "car"   :-: args)) = arrity 1 "car" car    =<< traverse evalTerm args
evalTerm (List (Symbol "eq?"   :-: args)) = arrity 2 "eq?" eq     =<< traverse evalTerm args
evalTerm (List (Symbol "cdr"   :-: args)) = arrity 1 "cdr" cdr    =<< traverse evalTerm args
evalTerm (List (Symbol "cond"  :-: args)) = cond               =<< traverse (evalTerm <=< quotePredicates) args
evalTerm (List (Symbol "add"   :-: args)) = Number . sum       <$> traverse (asInteger <=< evalTerm) args
evalTerm (List Nil)                       = List               <$> traverse evalTerm Nil
evalTerm (List xs)                        = badAppplication    =<< traverse evalTerm xs
evalTerm expr = return expr

asInteger :: MonadError EvalError m => Term -> m Integer
asInteger (Number n) = return n
asInteger term = throwError $ TypeError "asInteger" term

badAppplication :: MonadError EvalError m => DotList Term -> m Term
badAppplication (x :-: xs) = throwError $ ObjectNotApplicable x

quotePredicates :: MonadError EvalError m => Term -> m Term
quotePredicates (List (p :-: e :-: Nil)) = do
  p' <- evalTerm p
  return $ List (Symbol "quote" :-: (List (p' :-: (e :-: Nil)) :-: Nil))
quotePredicates _      = throwError IllFormedSyntax

cond :: MonadError EvalError m => DotList Term -> m Term
cond [] = throwError UnspecifiedReturn
cond (x :-: xs) =
  case x of
    List (Boolean pe :-: Nil)       -> if pe then return (Boolean pe) else cond xs
    List (Boolean p  :-: e :-: Nil) -> if p then return e else cond xs
    List _ -> throwError IllFormedSyntax
    DotList (Boolean p  :-: e :-: Nil) -> if p then return e else cond xs
    DotList (Boolean pe :-: Nil)       -> if pe then return (Boolean pe) else cond xs
    _ -> throwError IllFormedSyntax

eq :: MonadError EvalError m => m Arrity -> m Term
eq terms = terms >>= \case
  Binary t1 t2 -> return . Boolean $ t1 == t2

cons :: MonadError EvalError m => m Arrity -> m Term
cons terms = terms >>= \case
  (Binary x (List xs) )   -> return $ List (x :-: xs)
  (Binary x (DotList xs)) -> return $ DotList (x :-: xs)
  (Binary x y)            -> return $ DotList (x :-. y)

car :: MonadError EvalError m => m Arrity -> m Term
car term = term >>= \case
    Unary (List (x :-: xs))   -> return x
    Unary (DotList (x :-. y)) -> return x
    Unary term                -> throwError $ TypeError "car" term
       
cdr :: MonadError EvalError m => m Arrity -> m Term
cdr term = term >>= \case
    Unary (DotList (x :-. y)) -> return y
    Unary (List (x :-: xs))   -> return . List $ xs
    Unary term                -> throwError $ TypeError "cdr" term

unary :: DotList Term -> Arrity
unary (x :-: _) = Unary x
unary (x :-. _) = Unary x

binary :: DotList Term -> Arrity
binary (x :-: y :-: _) = Binary x y
binary (x :-: y :-. _) = Binary x y
binary (x :-. y)       = Binary x y
  
arrity :: MonadError EvalError m => Int -> String -> (m Arrity -> m Term) -> DotList Term -> m Term
arrity i name cont xs =
  if length xs /= i
  then throwError $ WrongArrity name i (length xs)
  else case i of
    1 -> cont . pure $ unary xs
    2 -> cont . pure $ binary xs
    _ -> cont . pure $ Nary xs

atom :: MonadError EvalError m => m Arrity -> m Term
atom mterm = mterm >>= \case
  Unary (DotList _) -> return $ Boolean False
  Unary (List _)    -> return $ Boolean False
  Unary _           -> return $ Boolean True

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

