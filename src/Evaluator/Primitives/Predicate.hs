{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Evaluator.Primitives.Predicate where

import Control.Monad.Except

import Evaluator.Types

and' :: MonadError EvalError m => [Term] -> m Term
and' = arrity 2 "&&" f
  where
    f mterm =
      mterm >>= \case
        Binary (Boolean p) (Boolean q) -> return . Boolean $ p && q
        Binary _ _ -> undefined
        Unary _ -> undefined


or' :: MonadError EvalError m => [Term] -> m Term
or' = arrity 2 "||" f
  where
    f mterm =
      mterm >>= \case
        Binary (Boolean p) (Boolean q) -> return . Boolean $ p || q
        Binary _ _ -> undefined
        Unary _ -> undefined

any' :: MonadError EvalError m => [Term] -> m Term
any' terms = return . Boolean $ f terms
  where f [Boolean x] = x
        f (Boolean x : xs)  = x || f xs
        f _                = False

all' :: MonadError EvalError m => [Term] -> m Term
all' terms = return . Boolean $ f terms
  where f [Boolean x] = x
        f (Boolean x : xs)  = x && f xs
        f _                = True

greater :: MonadError EvalError m => [Term] -> m Term
greater = arrity 2 ">" f
  where
    f mterm =
      mterm >>= \case
        Binary (Number x) (Number y) -> return . Boolean $ x > y
        Binary _ _ -> undefined
        Unary _ -> undefined

less :: MonadError EvalError m => [Term] -> m Term
less = arrity 2 "<" f
  where
    f mterm =
      mterm >>= \case
        Binary (Number x) (Number y) -> return . Boolean $ x < y
        Binary _ _ -> undefined
        Unary _ -> undefined
