{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Lib where

import Data.Typeable
import Data.Functor
import Data.List (intercalate)
import Data.Text (Text(..))
import Data.Text.Encoding (encodeUtf8)

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative

import System.Console.Haskeline

import Text.Trifecta
import Text.Parser.Combinators

data DotList a = a :-: (DotList a) | a :-. a
  deriving (Functor, Foldable, Traversable, Eq)

infixr 5 :-:
infixr 5 :-.

instance Show a => Show (DotList a) where
  show (v :-: vs)  = show v <> " " <> show vs
  show (a :-. b)   =  show a <> " . " <> show b

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
  | Nil
  | Error EvalError
  deriving Eq

data Arrity = Unary Term | Binary Term Term

instance Show Term where
    show (Symbol str) = str
    show (Number n) = show n
    show (String str) = show str
    show (Boolean bool) = show bool
    show (List xs) = "(" ++ show xs ++ ")"
    show (DotList xs) = "(" ++ show xs ++ ")"
    show (Error e) = show e
    show Nil = "()"

data ParseError = ParseError deriving Show

data EvalError
  = TypeError String Term
  | WrongArrity String Int Int
  | ObjectNotApplicable Term
  | NotAProperList Term
  | UnspecifiedReturn
  | IllFormedSyntax
  deriving Eq

-- | TODO: Improve Error Show instances
instance Show EvalError where
  show (TypeError func term) = "TypeError: The object " ++ show term ++ " passed to " ++ func ++ " is not the right type."
  show (WrongArrity func arrity params) = "The procedure " ++ func ++ " has been called with " ++ show params ++
                                      " arguments; it requires exactly " ++ show arrity ++ " argument"
  show (ObjectNotApplicable obj) = "The object '" ++ show obj ++ "' is not applicable."
  show (NotAProperList term) = "Combination must be a proper list: " ++ show term
  show UnspecifiedReturn = "Unspecified return value."
  show IllFormedSyntax = "Ill-formed syntax"

----------------
---- Parser ----
----------------
-- | TODO: Investigate possibility custom error reporting in Trifecta
-- | ()) should fail

parseNumber :: Parser Term
parseNumber = Number <$> integer

parseSymbol :: Parser Term
parseSymbol = (\x xs -> Symbol (x:xs)) <$> letter <*> many (alphaNum <|> char '?')

parseString' :: Parser Term
parseString' = String <$> between (char '"') (char '"') (some alphaNum)

parseBool :: Parser Term
parseBool = Boolean <$> (string "True" $> True <|> (string "False" $> False))

parseQuote :: Parser Term
parseQuote = (\x -> List (Symbol "quote" :-: x :-. Nil)) <$> (void (char '\'') *> parseTerm)

parseScalars :: Parser Term
parseScalars = parseQuote <|> parseNumber <|> parseString' <|> parseBool <|> parseSymbol 

parseRegList :: Parser Term
parseRegList = parens $ do
  xs <- parseTerm `sepBy` spaces
  if null xs
  then return Nil
  else return . List $ f xs
  where f [x] = x :-. Nil
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
-- various arithmetic
-- various logic

-- | Factor out the primitive function pattern matching into a global context object (ReaderT)
evalTerm :: (MonadState env m, MonadError EvalError m) => Term -> m Term
evalTerm (List (Symbol "add"   :-. Nil))  = return $ Number 0            -- special case for no arrity
evalTerm (List (Symbol "cond"  :-. Nil))  = throwError UnspecifiedReturn -- special case for no arrity
evalTerm (List (Symbol "quote" :-: value :-. Nil)) = return value
evalTerm (List (Symbol "atom?" :-: args)) = arrity 1 "atom?" atom =<< traverse evalTerm args
evalTerm (List (Symbol "cons"  :-: args)) = arrity 2 "cons" cons  =<< traverse evalTerm args
evalTerm (List (Symbol "car"   :-: args)) = arrity 1 "car" car    =<< traverse evalTerm args
evalTerm (List (Symbol "eq?"   :-: args)) = arrity 2 "eq?" eq     =<< traverse evalTerm args
evalTerm (List (Symbol "cdr"   :-: args)) = arrity 1 "cdr" cdr    =<< traverse evalTerm args
evalTerm (List (Symbol "cond"  :-: args)) = cond                  =<< traverse (evalTerm <=< quotePredicates) args
evalTerm (List (Symbol "add"   :-: args)) = add                   =<< traverse (asInteger <=< evalTerm) args
evalTerm (List xs)                        = badAppplication       =<< traverse evalTerm xs
evalTerm (DotList xs)                     = improperList          =<< traverse evalTerm xs
evalTerm expr = return expr

-- execEval :: String -> Result (Either EvalError Term)
-- execEval str = runExcept . evalTerm <$> parse str


--- | Error Handling

asInteger :: MonadError EvalError m => Term -> m Term
asInteger (Number n) = return $ Number n
asInteger Nil = return Nil
asInteger term = throwError $ TypeError "asInteger" term

badAppplication :: MonadError EvalError m => DotList Term -> m Term
badAppplication (x :-. xs) = throwError $ ObjectNotApplicable x
badAppplication (x :-: xs) = throwError $ ObjectNotApplicable x

improperList :: MonadError EvalError m => DotList Term -> m Term
improperList xs = throwError . NotAProperList $ DotList xs

quotePredicates :: (MonadState env m, MonadError EvalError m) => Term -> m Term
quotePredicates (List (p :-: e :-. Nil)) = do
  p' <- evalTerm p
  return $ List (Symbol "quote" :-: (List (p' :-: (e :-. Nil)) :-. Nil))
quotePredicates (List (p :-. Nil)) = do
  p' <- evalTerm p
  return $ List (Symbol "quote" :-: (DotList $ p' :-. Nil) :-. Nil)
quotePredicates Nil = return Nil
quotePredicates x = throwError IllFormedSyntax

unary :: DotList Term -> Arrity
unary (x :-: _) = Unary x
unary (x :-. _) = Unary x

binary :: DotList Term -> Arrity
binary (x :-: y :-: _) = Binary x y
binary (x :-: y :-. _) = Binary x y
binary (x :-. y)       = Binary x y
  
arrity :: MonadError EvalError m => Int -> String -> (m Arrity -> m Term) -> DotList Term -> m Term
arrity i name cont xs =
  if length xs /= i + 1
  then throwError $ WrongArrity name i (length xs - 1)
  else case i of
    1 -> cont . pure $ unary xs
    2 -> cont . pure $ binary xs

--- | Arithmetic

add :: MonadError EvalError m => DotList Term -> m Term
add terms = return . Number $ f terms
  where f (Number x :-. Nil) = x
        f (Number x :-: xs)  = x + f xs
        f _                = 0

subtract :: MonadError EvalError m => DotList Term -> m Term
subtract terms = return . Number $ f terms
  where f = undefined

multiply :: MonadError EvalError m => DotList Term -> m Term
multiply terms = return . Number $ f terms
  where f = undefined

divide :: MonadError EvalError m => DotList Term -> m Term
divide terms = return . Number $ f terms
  where f = undefined

abs :: MonadError EvalError m => DotList Term -> m Term
abs terms = return . Number $ f terms
  where f = undefined

modulo :: MonadError EvalError m => DotList Term -> m Term
modulo terms = return . Number $ f terms
  where f = undefined

negate :: MonadError EvalError m => DotList Term -> m Term
negate terms = return . Number $ f terms
  where f = undefined

signum :: MonadError EvalError m => DotList Term -> m Term
signum terms = return . Number $ f terms
  where f = undefined

--- | Logic
and :: MonadError EvalError m => DotList Term -> m Term
and terms = return . Boolean $ f terms
  where f = undefined


or :: MonadError EvalError m => DotList Term -> m Term
or terms = return . Boolean $ f terms
  where f = undefined

any :: MonadError EvalError m => DotList Term -> m Term
any terms = return . Boolean $ f terms
  where f = undefined

all :: MonadError EvalError m => DotList Term -> m Term
all terms = return . Boolean $ f terms
  where f = undefined

greater :: MonadError EvalError m => DotList Term -> m Term
greater terms = return . Boolean $ f terms
  where f = undefined

less :: MonadError EvalError m => DotList Term -> m Term
less terms = return . Boolean $ f terms
  where f = undefined

--- | McCarthy Primitives

-- | TODO: cond needs to be simplified drastically
cond :: (MonadState env m, MonadError EvalError m) => DotList Term -> m Term
cond (x :-. Nil) =
  case x of
    List (Boolean p  :-: e :-. Nil) -> if p then evalTerm e else throwError UnspecifiedReturn
    List (Number n  :-: e :-. Nil)  -> evalTerm e
    DotList (pe :-. Nil) -> return pe
    x -> throwError IllFormedSyntax
cond (x :-: xs) =
  case x of
    List (Boolean pe :-. Nil)       -> if pe then return (Boolean pe) else cond xs
    List (Boolean p  :-: e :-. Nil) -> if p then evalTerm e else cond xs
    List (Number n  :-: e :-. Nil) -> evalTerm e
    List _ -> throwError IllFormedSyntax
    DotList (Boolean p  :-: e :-. Nil) -> if p then evalTerm e else cond xs
    DotList (Boolean pe :-. Nil)       -> if pe then return (Boolean pe) else cond xs
    Nil -> throwError UnspecifiedReturn
    x -> throwError IllFormedSyntax
cond _ = throwError IllFormedSyntax

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

atom :: MonadError EvalError m => m Arrity -> m Term
atom mterm = mterm >>= \case
  Unary (DotList xs) -> throwError $ NotAProperList (DotList xs)
  Unary (List _)     -> return     $ Boolean False
  Unary _            -> return     $ Boolean True

lambda :: MonadError EvalError m => m Arrity -> m Term
lambda mterm = mterm >>= \case
  Binary args body -> undefined
  

------------
--- REPL ---
------------

--type Repl a = InputT IO a

-- repl = forever $ do
--   rawInput <- getInputLine "> "
--   case rawInput of
--     Nothing -> repl
--     Just input -> do
--        let parsedResult = parse input
--        let evalResult = execEval input
--        liftIO $ print evalResult


--class MonadIO m => MonadRepl m where
--  readRepl :: String -> m String
--
--class Monad m => MonadEval m where
--  eval :: Term -> m Term

--instance MonadEval (LispM env) where
--  eval term = evalTerm term `catchError` (return . Error)


--instance MonadRepl (LispM env) where
--  readRepl str = do
--    rawInput <- LispM . lift . lift $ getInputLine str
--    case rawInput of
--      Just input -> return input
--      Nothing -> return mempty


--repl :: Env' -> IO ()
--repl env = do
--  env' <- runLispM env $ do
--    -- Testing state:
--    Env' state <- get
--    --liftIO $ print state
--    put . Env' $ ():state 
--
--    rawInput <- readRepl "> "
--    p <- parse rawInput 
--    term <- eval p
--    liftIO $ print term
--    return term
--  --print env'
--  repl env'


-------------
--- LispM ---
-------------

newtype EvalEnv = EvalEnv [()]

newtype LispM env a = LispM { unLispM :: ExceptT EvalError (State env) a}
  deriving (Functor, Applicative, Monad, MonadState env, MonadError EvalError)

runLispM :: EvalEnv -> LispM EvalEnv Term -> Either EvalError Term
runLispM env = flip evalState env . runExceptT . unLispM 

eval :: Result Term -> LispM EvalEnv Term
eval (Success term) = evalTerm term
eval (Failure err)  = throwError IllFormedSyntax

------------
--- AppM ---
------------

data Env = Env { _source :: Text, _logLevel :: LogLevel }

newtype AppM m a = AppM { unAppM :: ReaderT Env m a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

runAppM :: Env -> AppM m a -> m a
runAppM env (AppM m) = runReaderT m env

interpret :: AppM IO (Either EvalError Term)
interpret = do
  source <- asks _source
  let evalEnv = EvalEnv []
  let lispExpression = parse source
  return . runLispM evalEnv $ eval lispExpression
  

--------------------
--- Capabilities ---
--------------------
-- How does a repl relate to these capabilities?

data LogLevel = Normal | Debug

class Monad m => MonadLog m where
  logMessage :: LogLevel -> Text -> m ()

logNormal :: MonadLog m => Text -> m ()
logNormal = logMessage Normal

logDebug :: MonadLog m => Text -> m ()
logDebug = logMessage Debug
