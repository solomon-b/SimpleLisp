{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Evaluator.Primitives.McCarthy where

import Control.Monad.Except
import Control.Monad.State

import Evaluator.Types

quote :: (MonadEnv m, MonadState EvalEnv m, MonadError EvalError m) => DotList Term -> m Term
quote = arrity 1 "quote?" f
  where
    f mterm =
      mterm >>= \case
        Unary xs -> return xs
        Binary _ _ -> undefined

-- | TODO: cond needs to be simplified drastically
cond :: (MonadEnv m, MonadState EvalEnv m, MonadError EvalError m) => (Term -> m Term) -> DotList Term -> m Term
cond f (x :-. Nil) =
  case x of
    List (Boolean p  :-: e :-. Nil) -> if p then f e else throwError UnspecifiedReturn
    List (Number _  :-: e :-. Nil)  -> f e
    DotList (pe :-. Nil) -> return pe
    _ -> throwError IllFormedSyntax
cond f (x :-: xs) =
  case x of
    List (Boolean pe :-. Nil)       -> if pe then return (Boolean pe) else cond f xs
    List (Boolean p  :-: e :-. Nil) -> if p then f e else cond f xs
    List (Number _  :-: e :-. Nil) -> f e
    List _ -> throwError IllFormedSyntax
    DotList (Boolean p  :-: e :-. Nil) -> if p then f e else cond f xs
    DotList (Boolean pe :-. Nil)       -> if pe then return (Boolean pe) else cond f xs
    Nil -> throwError UnspecifiedReturn
    _ -> throwError IllFormedSyntax
cond _ _ = throwError IllFormedSyntax

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


reduceArgs :: DotList Term -> [Either EvalError String]
reduceArgs (Symbol str :-: ts) = Right str : reduceArgs ts
reduceArgs (_ :-: ts) = Left IllFormedSyntax : reduceArgs ts
reduceArgs (Symbol s1 :-. Symbol s2) = [Right s1, Right s2]
reduceArgs (_ :-. Symbol s1) = [Left IllFormedSyntax, Right s1]
reduceArgs (Symbol s1 :-. Nil) = [Right s1]
reduceArgs (Symbol s1 :-. _) = [Right s1, Left IllFormedSyntax]
reduceArgs (_ :-. _) = [Left IllFormedSyntax]

-- | TODO: Implement dat lambda
lambda :: MonadError EvalError m => DotList Term -> m Term
lambda = arrity 2 "lambda" f
  where
    f mterm = mterm >>= \case
      Binary (List args) body -> do
        case sequence $ reduceArgs args of
          Left err -> throwError err
          Right xs -> return $ Func xs Nothing body
      Binary _ _ -> undefined -- throwError IllFormedSyntax
      Unary _    -> undefined

define :: (MonadEnv m, MonadError EvalError m) => (Term -> m Term) -> DotList Term -> m Term
define f = arrity 2 "define" g
  where
    g mterm = mterm >>= \case
      Binary (Symbol var) val -> do
        val' <- f val
        putVar var val'
      _ -> throwError IllFormedSyntax

  
