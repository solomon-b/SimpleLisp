{-# LANGUAGE FlexibleContexts #-}
module Evaluator where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.State

import Data.Bifunctor (first)

import Text.Trifecta (Result(..))

import Evaluator.Types
import Evaluator.Primitives.Arithmetic
import Evaluator.Primitives.McCarthy
import Evaluator.Primitives.Predicate


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
-- define ✓
-- lambda ✓
-- label
-- fix :D

evalTerm :: (MonadEnv m, MonadState env m, MonadError EvalError m) => Term -> m Term
evalTerm (Symbol "+") = throwError IllFormedSyntax
evalTerm (Symbol str) = readVar str
evalTerm (List []) = return $ List []
evalTerm (List xs) =
  let (op, args) = first (\x -> parseArithOp x <|> parsePredOp x <|> parsePrim x) (head xs, tail xs)
  in case op of
    Just (Arith arithOp)   -> evalArith (arithOp, args)
    Just (Pred predOp)     -> evalPred (predOp, args)
    Just (McCarthy primOp) -> evalPrim (primOp, args)
    Nothing                -> (badApp . List) =<< traverse evalTerm xs
evalTerm (DotList (xs, x)) = improperList . DotList =<< traverse evalTerm (xs, x)
evalTerm term = return term

data McCarthyOp = Atom | Cons | Car | Eq | Cdr | Define | Cond | Quote | Lambda Term | Apply | Lookup String
data ArithOp    = Add | Subtract | Multiply | Divide | ABS | Modulo | Signum | Negate
data PredOp     = And | Or | Any | All | Greater | Less

data Primitive = McCarthy McCarthyOp | Arith ArithOp | Pred PredOp

parsePrim :: Term -> Maybe Primitive
parsePrim (Symbol str) =
  case str of
    "atom?"  -> Just $ McCarthy Atom
    "cons"   -> Just $ McCarthy Cons
    "car"    -> Just $ McCarthy Car
    "eq?"    -> Just $ McCarthy Eq
    "cdr"    -> Just $ McCarthy Cdr
    "define" -> Just $ McCarthy Define
    "cond"   -> Just $ McCarthy Cond
    "quote"  -> Just $ McCarthy Quote
    "apply"  -> Just $ McCarthy Apply
    _        -> Just $ McCarthy (Lookup str)
parsePrim term@(Func _ _) = Just . McCarthy $ Lambda term
parsePrim _ =   Nothing

parsePredOp :: Term -> Maybe Primitive
parsePredOp (Symbol str) =
  case str of
    "and"     -> Just $ Pred And
    "or"      -> Just $ Pred Or
    "any"     -> Just $ Pred Any
    "all"     -> Just $ Pred All
    ">"       -> Just $ Pred Greater
    "<"       -> Just $ Pred Less
    _         -> Nothing
parsePredOp _ = Nothing

parseArithOp :: Term -> Maybe Primitive
parseArithOp (Symbol str) =
  case str of
    "+"       -> Just $ Arith Add
    "-"       -> Just $ Arith Subtract
    "*"       -> Just $ Arith Multiply
    "/"       -> Just $ Arith Divide
    "%"       -> Just $ Arith Modulo
    "abs"     -> Just $ Arith ABS
    "signum"  -> Just $ Arith Signum
    "negate"  -> Just $ Arith Negate
    _         -> Nothing
parseArithOp _ = Nothing

--lookupFunction :: (MonadEnv m, MonadError EvalError m) => [String] -> m Term
--lookupFunction (x:xs) = do
--  var <- readVar x
--  case var of
--    f@(Func _ _) -> applyLambda evalTerm f:(evalTerm xs) 
--    term -> badApp term

evalPrim :: (MonadEnv m, MonadError EvalError m) => (McCarthyOp, [Term]) -> m Term
evalPrim (op, args) =
  case op of
    Atom        -> f atom
    Cons        -> f cons
    Car         -> f car
    Eq          -> f eq
    Cdr         -> f cdr
    Define      -> h $ define evalTerm
    Cond        -> g $ cond evalTerm
    Quote       -> h quote 
    Lambda func -> applyLambda evalTerm (func:args)
    Apply       -> f $ apply evalTerm
    Lookup str  -> i str
  where
    f op' =
      case args of
        []    -> throwError IllFormedSyntax
        args' -> op' =<< traverse evalTerm args'
    g op' =
      case args of
        []    -> throwError UnspecifiedReturn
        args' -> op' =<< traverse (evalTerm <=< quotePredicates) args'
    h op' = 
      case args of
        []    -> throwError IllFormedSyntax
        args' -> op' args'
    i var = do
      val <- readVar var
      case val of
        f@(Func _ _) -> applyLambda evalTerm (f:args) 
        term -> badApp term
        

evalArith :: (MonadEnv m, MonadError EvalError m) => (ArithOp, [Term]) -> m Term
evalArith (op, args) =
  case op of
    Add      -> f add 0
    Subtract -> g subtract'
    Multiply -> f multiply 1
    Divide   -> g divide
    ABS      -> g abs'
    Modulo   -> g modulo
    Signum   -> g signum'
    Negate   -> g negate'
  where
    f op' identity = 
      case args of
        [] -> return $ Number identity
        [Number i] -> return $ Number i
        args' -> op' =<< traverse (asInteger <=< evalTerm) args'
    g op' =
      case args of
        []  -> throwError IllFormedSyntax
        args' -> op' =<< traverse (asInteger <=< evalTerm) args'

evalPred :: (MonadEnv m, MonadError EvalError m) => (PredOp, [Term]) -> m Term
evalPred (op, args) =
  case op of
    And     -> f and'
    Or      -> f or'
    Any     -> f any'
    All     -> f all'
    Greater -> f greater
    Less    -> f less
  where
    f op' =
      case args of
        [] -> throwError IllFormedSyntax
        args' -> op' =<< traverse (evalTerm <=< quotePredicates) args'

eval :: Result Term -> LispM EvalEnv Term
eval (Success term) = evalTerm term
eval (Failure _)  = throwError IllFormedSyntax


------------------
--- Validators ---
------------------

asInteger :: MonadError EvalError m => Term -> m Term
asInteger (Number n) = return $ Number n
asInteger (List []) = return (List [])
asInteger term = throwError $ TypeError "asInteger" term

badApp :: MonadError EvalError m => Term -> m Term
badApp (DotList (x:_, _)) = throwError $ ObjectNotApplicable x
badApp (DotList ([], _)) = throwError IllFormedSyntax
badApp (List (x:_)) = throwError $ ObjectNotApplicable x
badApp term = pure term

improperList :: MonadError EvalError m => Term -> m Term
improperList (DotList (xs, x)) = throwError . NotAProperList $ DotList (xs, x)
improperList term = pure term

quotePredicates :: (MonadEnv m, MonadState env m, MonadError EvalError m) => Term -> m Term
quotePredicates (List [p, e]) = do
  p' <- evalTerm p
  return $ List [Symbol "quote", List [p', e]]
quotePredicates (List [pe]) = do
  pe' <- evalTerm pe
  return $ List [Symbol "quote", List [pe']]
quotePredicates (List []) = return $ List []
quotePredicates _ = throwError IllFormedSyntax
