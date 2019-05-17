{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Lib where

import Data.Functor
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative

import Text.Trifecta
import System.Console.Haskeline

data DotList a = a :-: (DotList a) | a :-. a
  deriving (Functor, Foldable, Traversable, Eq)

infixr 5 :-:
infixr 5 :-.

instance Show a => Show (DotList a) where
  show (v :-: vs)  = show v <> " " <> show vs
  show (a :-. b)   =  show a <> " . " <> show b

listToDot :: [a] -> DotList a
listToDot [] = undefined
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
  | Func [String] (Maybe String) [Term]
  deriving Eq

data Arrity = Unary Term | Binary Term Term

instance Show Term where
    show (Symbol str) = "SYMBOL: " ++ str
    show (Number n) = show n
    show (String str) = "STRING: " ++ show str
    show (Boolean bool) = "BOOL: " ++ show bool
    show (List xs) = "(" ++ show xs ++ ")"
    show (DotList xs) = "(" ++ show xs ++ ")"
    show (Error e) = show e
    show Nil = "()"
    show (Func args varargs _) =
      "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"

data ParseError = ParseError deriving Show

data EvalError
  = TypeError String Term
  | WrongArrity String Int Int
  | ObjectNotApplicable Term
  | NotAProperList Term
  | UnspecifiedReturn
  | IllFormedSyntax
  | UnboundVariable
  deriving Eq

-- | TODO: Improve Error Show instances
instance Show EvalError where
  show (TypeError func term) = "TypeError: The object " ++ show term ++ " passed to " ++ func ++ " is not the right type."
  show (WrongArrity func arrity' params) = "The procedure " ++ func ++ " has been called with " ++ show params ++
                                      " arguments; it requires exactly " ++ show arrity' ++ " argument"
  show (ObjectNotApplicable obj) = "The object '" ++ show obj ++ "' is not applicable."
  show (NotAProperList term) = "Combination must be a proper list: " ++ show term
  show UnspecifiedReturn = "Unspecified return value."
  show IllFormedSyntax = "Ill-formed syntax"
  show UnboundVariable = "Unbound Variable"

----------------
---- Parser ----
----------------
-- | TODO: Investigate possibility custom error reporting in Trifecta
-- | ()) should fail

parseNumber :: Parser Term
parseNumber = Number <$> integer

parseSymbol :: Parser Term
parseSymbol = do
  x <- letter <|> oneOf "?+"
  xs <- many (alphaNum <|> oneOf "?+")
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
-- define ✓
-- fix :D
-- various arithmetic
-- various logic

-- | Factor out the primitive function pattern matching into a global context object (ReaderT)
evalTerm :: (MonadEnv m, MonadState EvalEnv m, MonadError EvalError m) => Term -> m Term
evalTerm (List (Symbol "+"    :-. Nil))  = return $ Number 0            -- special case for no arrity
evalTerm (List (Symbol "cond"   :-. Nil))  = throwError UnspecifiedReturn -- special case for no arrity
evalTerm (List (Symbol "quote"  :-: value :-. Nil)) = return value
evalTerm (List (Symbol "atom?"  :-: args)) = atom     =<< traverse evalTerm args
evalTerm (List (Symbol "cons"   :-: args)) = cons     =<< traverse evalTerm args
evalTerm (List (Symbol "car"    :-: args)) = car      =<< traverse evalTerm args
evalTerm (List (Symbol "eq?"    :-: args)) = eq       =<< traverse evalTerm args
evalTerm (List (Symbol "cdr"    :-: args)) = cdr      =<< traverse evalTerm args
evalTerm (List (Symbol "cond"   :-: args)) = cond     =<< traverse (evalTerm  <=< quotePredicates) args
evalTerm (List (Symbol "+"    :-: args))   = add      =<< traverse (asInteger <=< evalTerm)        args
evalTerm (List (Symbol "define" :-: args)) = define args
evalTerm (List xs)                         = badApp   =<< traverse evalTerm xs
evalTerm (DotList xs)                      = improperList =<< traverse evalTerm xs
evalTerm (Symbol "+") = throwError IllFormedSyntax
evalTerm (Symbol str) = getVar str 
evalTerm expr = return expr

primitives :: (MonadEnv m, MonadError EvalError m) => [(String, DotList Term -> m Term)]
primitives =
  [ ("atom?", atom)
  , ("cons", cons)
  , ("car", car)
  , ("eq?", eq)
  , ("cdr", cdr)
  , ("define", define)
  , ("cond", cond)
  , ("+", add)
  ]


  
getPrimitive :: (MonadEnv m, MonadError EvalError m) => String -> m (DotList Term -> m Term)
getPrimitive str = maybe (throwError IllFormedSyntax) return $ lookup str primitives

--- | Error Handling
asInteger :: MonadError EvalError m => Term -> m Term
asInteger (Number n) = return $ Number n
asInteger Nil = return Nil
asInteger term = throwError $ TypeError "asInteger" term

badApp :: MonadError EvalError m => DotList Term -> m Term
badApp (x :-. _) = throwError $ ObjectNotApplicable x
badApp (x :-: _) = throwError $ ObjectNotApplicable x

improperList :: MonadError EvalError m => DotList Term -> m Term
improperList xs = throwError . NotAProperList $ DotList xs

quotePredicates :: (MonadEnv m, MonadState EvalEnv m, MonadError EvalError m) => Term -> m Term
quotePredicates (List (p :-: e :-. Nil)) = do
  p' <- evalTerm p
  return $ List (Symbol "quote" :-: (List (p' :-: (e :-. Nil)) :-. Nil))
quotePredicates (List (p :-. Nil)) = do
  p' <- evalTerm p
  return $ List (Symbol "quote" :-: (DotList $ p' :-. Nil) :-. Nil)
quotePredicates Nil = return Nil
quotePredicates _ = throwError IllFormedSyntax

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
    j -> throwError $ WrongArrity name j (length xs - 1) -- Verify this  error message makes sense

--- | Arithmetic
-- | TODO: Implement maths

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
cond :: (MonadEnv m, MonadState EvalEnv m, MonadError EvalError m) => DotList Term -> m Term
cond (x :-. Nil) =
  case x of
    List (Boolean p  :-: e :-. Nil) -> if p then evalTerm e else throwError UnspecifiedReturn
    List (Number _  :-: e :-. Nil)  -> evalTerm e
    DotList (pe :-. Nil) -> return pe
    _ -> throwError IllFormedSyntax
cond (x :-: xs) =
  case x of
    List (Boolean pe :-. Nil)       -> if pe then return (Boolean pe) else cond xs
    List (Boolean p  :-: e :-. Nil) -> if p then evalTerm e else cond xs
    List (Number _  :-: e :-. Nil) -> evalTerm e
    List _ -> throwError IllFormedSyntax
    DotList (Boolean p  :-: e :-. Nil) -> if p then evalTerm e else cond xs
    DotList (Boolean pe :-. Nil)       -> if pe then return (Boolean pe) else cond xs
    Nil -> throwError UnspecifiedReturn
    _ -> throwError IllFormedSyntax
cond _ = throwError IllFormedSyntax

eq :: MonadError EvalError m => DotList Term -> m Term
eq = arrity 2 "eq?" f
  where
    f mterm = mterm >>= \case
      Binary t1 t2 -> return . Boolean $ t1 == t2
      Unary _ -> undefined

cons :: MonadError EvalError m => DotList Term -> m Term
cons = arrity 2 "cons" f
  where
    f terms = terms >>= \case
      (Binary x (List xs) )   -> return $ List (x :-: xs)
      (Binary x (DotList xs)) -> return $ DotList (x :-: xs)
      (Binary x y)            -> return $ DotList (x :-. y)
      Unary _ -> undefined

car :: MonadError EvalError m => DotList Term -> m Term
car = arrity 1 "car" f
  where
    f term = term >>= \case
      Unary (List (x :-: _))    -> return x
      Unary (DotList (x :-. _)) -> return x
      Unary term'               -> throwError $ TypeError "car" term'
      Binary _ _ -> undefined
       
cdr :: MonadError EvalError m => DotList Term -> m Term
cdr = arrity 1 "cdr" f
  where
    f term = term >>= \case
      Unary (DotList (_ :-. y)) -> return y
      Unary (List (_ :-: xs))   -> return . List $ xs
      Unary term'               -> throwError $ TypeError "cdr" term'
      Binary _ _ -> undefined

atom :: MonadError EvalError m => DotList Term -> m Term
atom = arrity 1 "atom?" f
  where
    f mterm =
      mterm >>= \case
        Unary (DotList xs) -> throwError $ NotAProperList (DotList xs)
        Unary (List _)     -> return     $ Boolean False
        Unary _            -> return     $ Boolean True
        Binary _ _ -> undefined

-- | TODO: Implement dat lambda
lambda :: MonadError EvalError m => m Arrity -> m Term
lambda mterm = mterm >>= \case
  Binary args body -> undefined
  Unary _ -> undefined

getVar :: (MonadEnv m, MonadError EvalError m) => String -> m Term
getVar = readVar

setVar :: (MonadEnv m, MonadError EvalError m) => String -> Term -> m Term
setVar str term = readVar str >>= \_ -> putVar str term

bindVars :: (MonadEnv m, MonadError EvalEnv m) => [(String, Term)] -> m EvalEnv
bindVars xs = mapM_ (uncurry putVar) xs *> get

define :: (MonadEnv m, MonadError EvalError m) => DotList Term -> m Term
define = arrity 2 "define" f
  where
    f mterm = mterm >>= \case
      Binary (Symbol var) val -> putVar var val >>= evalTerm
      _ -> throwError IllFormedSyntax

  
------------
--- REPL ---
------------

repl :: EvalEnv -> IO ()
repl initialEnv = runInputT defaultSettings (loop initialEnv)
  where loop env = do
          mstr <- getInputLine "> "
          case mstr of
            Just str -> do
              let (res, env') = runLispM env . eval . parse $ pack str
              liftIO $ print res
              loop env'
            Nothing -> loop env


-------------
--- LispM ---
-------------

newtype EvalEnv = EvalEnv (Map String Term) deriving Show

evalEnv :: EvalEnv
evalEnv = EvalEnv M.empty

newtype LispT env m a = LispT { unLispT :: ExceptT EvalError (StateT env m) a}
  deriving (Functor, Applicative, Monad, MonadState env, MonadError EvalError)

instance MonadTrans (LispT env) where
  lift :: Monad m => m a -> LispT env m a
  lift = LispT . lift . lift
  --lift ma = LispT . ExceptT . StateT $ \env -> (\a -> (Right a, env)) <$> ma

type LispM env = LispT env Identity

runLispM :: EvalEnv -> LispM EvalEnv Term -> (Either EvalError Term, EvalEnv)
runLispM env = flip runState env . runExceptT . unLispT

evalLispM :: EvalEnv -> LispM EvalEnv Term -> Either EvalError Term
evalLispM env = flip evalState env . runExceptT . unLispT

execLispM :: EvalEnv -> LispM EvalEnv Term -> EvalEnv
execLispM env = flip execState env . runExceptT . unLispT

eval :: Result Term -> LispM EvalEnv Term
eval (Success term) = evalTerm term
eval (Failure _)  = throwError IllFormedSyntax


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
  sourceCode <- asks _source
  let lispExpression = parse sourceCode
  return . evalLispM evalEnv $ eval lispExpression
  

--------------------
--- Capabilities ---
--------------------
-- How does a repl relate to these capabilities?

-- | TODO: Implement logging
data LogLevel = Normal | Debug

class Monad m => MonadLog m where
  logMessage :: LogLevel -> Text -> m ()

logNormal :: MonadLog m => Text -> m ()
logNormal = logMessage Normal

logDebug :: MonadLog m => Text -> m ()
logDebug = logMessage Debug

class (MonadState EvalEnv m, MonadError EvalError m) => MonadEnv m where
  readVar :: String -> m Term
  putVar :: String -> Term -> m Term

instance MonadEnv (LispM EvalEnv) where
  readVar str = get >>= \(EvalEnv env) -> maybe (throwError UnboundVariable) return (M.lookup str env) >>= evalTerm
  putVar str term = (get >>= \(EvalEnv env) -> put . EvalEnv $ M.insert str term env) >> return term


