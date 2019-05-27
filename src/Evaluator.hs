{-# LANGUAGE FlexibleContexts #-}
module Evaluator where

import Control.Monad.Except
import Control.Monad.State

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
-- lambda
-- label
-- define ✓
-- fix :D
-- various arithmetic
-- various logic

fromDotList :: DotList a -> (a, Either a (DotList a))
fromDotList (x :-. y) = (x, Left y)
fromDotList (x :-: y) = (x, Right y)

-- | Factor out the primitive function pattern matching into a global context object (ReaderT)
evalTerm :: (MonadEnv m, MonadState env m, MonadError EvalError m) => Term -> m Term
evalTerm (Symbol "+") = throwError IllFormedSyntax
evalTerm (Symbol str) = readVar str
evalTerm (List xs) =
  let (op, args) = fromDotList xs
  in case parseArithOp op of
        Just arithOp -> evalArithmetic (arithOp, args)
        Nothing ->
          case parsePrim op of
            Just primOp -> evalPrim (primOp, args)
            Nothing     -> badApp =<< traverse evalTerm xs
evalTerm (DotList xs) = improperList =<< traverse evalTerm xs
evalTerm term = return term

evalPrim :: (MonadEnv m, MonadError EvalError m) => (PrimOp, Either Term (DotList Term)) -> m Term
evalPrim (op, args) =
  case op of
    Atom   -> f atom
    Cons   -> f cons
    Car    -> f car
    Eq     -> f eq
    Cdr    -> f cdr
    Define -> h $ define evalTerm
    Cond   -> g $ cond evalTerm
    Quote  -> h quote 
  where f op' =
          case args of
            Left _ -> throwError IllFormedSyntax
            Right args' -> op' =<< traverse evalTerm args'
        g op' =
          case args of
            Left _ -> throwError UnspecifiedReturn
            Right args' -> op' =<< traverse (evalTerm <=< quotePredicates) args'
        h op' = 
          case args of
            Left _ -> throwError IllFormedSyntax
            Right args' -> op' args'

  
evalArithmetic :: (MonadEnv m, MonadError EvalError m) => (ArithOp, Either Term (DotList Term)) -> m Term
evalArithmetic (op, args) =
  case op of
    Add      -> f add 0
    Subtract -> g subtract'
    Multiply -> f multiply 1
    Divide   -> g divide
    ABS      -> g abs'
    Modulo   -> g modulo
    Signum   -> g signum'
    Negate   -> g negate'
  where f op' identity = 
          case args of
            Left Nil -> return $ Number identity
            Left (Number i) -> return $ Number i
            Left _  -> throwError IllFormedSyntax
            Right args' -> op' =<< traverse (asInteger <=< evalTerm) args'
        g op' =
          case args of
            Left _  -> throwError IllFormedSyntax
            Right args' -> op' =<< traverse (asInteger <=< evalTerm) args'

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

quotePredicates :: (MonadEnv m, MonadState env m, MonadError EvalError m) => Term -> m Term
quotePredicates (List (p :-: e :-. Nil)) = do
  p' <- evalTerm p
  return $ List (Symbol "quote" :-: (List (p' :-: (e :-. Nil)) :-. Nil))
quotePredicates (List (p :-. Nil)) = do
  p' <- evalTerm p
  return $ List (Symbol "quote" :-: (DotList $ p' :-. Nil) :-. Nil)
quotePredicates Nil = return Nil
quotePredicates _ = throwError IllFormedSyntax


eval :: Result Term -> LispM EvalEnv Term
eval (Success term) = evalTerm term
eval (Failure _)  = throwError IllFormedSyntax
