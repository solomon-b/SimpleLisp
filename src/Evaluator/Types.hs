{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Evaluator.Types where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
 
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


---------------
--- DotList ---
---------------

data DotList a = a :-: (DotList a) | a :-. a
  deriving (Functor, Foldable, Traversable, Eq)

-- (a, Either a (DotList a))

infixr 5 :-:
infixr 5 :-.

instance Show a => Show (DotList a) where
  show (v :-: vs)  = show v <> " " <> show vs
  show (a :-. b)   =  show a <> " . " <> show b

listToDot :: [a] -> DotList a
listToDot [] = undefined
listToDot [t, t'] = t :-. t'
listToDot (t:ts) = t :-: listToDot ts

fromDotList :: DotList a -> (a, Either a (DotList a))
fromDotList (x :-. y) = (x, Left y)
fromDotList (x :-: y) = (x, Right y)

headDL :: DotList a -> a
headDL (x :-. _) = x
headDL (x :-: _) = x

------------
--- Term ---
------------
  
data Term
  = Symbol String
  | Number Integer
  | String String
  | Boolean Bool
  | List (DotList Term)
  | DotList (DotList Term)
  | Nil
  | Error EvalError
  | Func [String] Term
  -- | Prim Primitive
  deriving Eq

instance Show Term where
    show (Symbol str) = str
    show (Number n) = show n
    show (String str) = show str
    show (Boolean bool) = show bool
    show (List xs) = "(" ++ show xs ++ ")"
    show (DotList xs) = "(" ++ show xs ++ ")"
    show (Error e) = show e
    show Nil = "()"
    show (Func args body) = "(lambda (" ++ unwords (map show args) ++ ")" ++ show body


-----------------
--- EvalError ---
-----------------

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


--------------
--- Arrity ---
--------------

data Arrity = Unary Term | Binary Term Term

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


---------------
--- EvalEnv ---
---------------

newtype EvalEnv = EvalEnv (Map String Term) deriving Show

evalEnv :: EvalEnv
evalEnv = EvalEnv M.empty


---------------
--- LispT/M ---
---------------

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


----------------
--- MonadEnv ---
----------------

class (MonadState EvalEnv m, MonadError EvalError m) => MonadEnv m where
  readVar :: String -> m Term
  putVar :: String -> Term -> m Term

instance MonadEnv (LispM EvalEnv) where
  readVar str =
    get >>= \(EvalEnv env) -> maybe (throwError UnboundVariable) return (M.lookup str env)
  putVar str term = (get >>= \(EvalEnv env) -> put . EvalEnv $ M.insert str term env) >> return term


setVar :: MonadEnv m => String -> Term -> m Term
setVar str term = readVar str >>= \_ -> putVar str term

bindVars :: MonadEnv m => [(String, Term)] -> m EvalEnv
bindVars xs = mapM_ (uncurry putVar) xs *> get


----------------
--- MonadLog ---
----------------

-- | TODO: Implement logging
data LogLevel = Normal | Debug

class Monad m => MonadLog m where
  logMessage :: LogLevel -> Text -> m ()

logNormal :: MonadLog m => Text -> m ()
logNormal = logMessage Normal

logDebug :: MonadLog m => Text -> m ()
logDebug = logMessage Debug
