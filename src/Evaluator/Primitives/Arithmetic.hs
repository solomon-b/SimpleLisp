{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Evaluator.Primitives.Arithmetic where

import Control.Monad.Except

import Evaluator.Types


add :: MonadError EvalError m => [Term] -> m Term
add terms = return . Number $ f terms
  where f [Number x] = x
        f (Number x : xs) = x + f xs
        f _ = 0

subtract' :: MonadError EvalError m => [Term] -> m Term
subtract' [Number x] = return . Number $ (- x)
subtract' terms = return . Number $ f terms
  where f (Number x : xs) = x - f xs
        f _ = 0

multiply :: MonadError EvalError m => [Term] -> m Term
multiply terms = return . Number $ f terms
  where f [Number x] = x
        f (Number x : xs)  = x * f xs
        f _                = 1

divide :: MonadError EvalError m => [Term] -> m Term
divide = arrity 2 "/" f
  where
    f mterm =
      mterm >>= \case
        Unary _ -> undefined
        Binary (Number x) (Number y) -> return . Number $ x `div` y
        Binary _ _ -> undefined

abs' :: MonadError EvalError m => [Term] -> m Term
abs' = arrity 1 "abs" f
  where
    f mterm =
      mterm >>= \case
        Unary (Number x) -> return . Number $ abs x
        Unary _ -> undefined
        Binary _ _ -> undefined

modulo :: MonadError EvalError m => [Term] -> m Term
modulo = arrity 2 "%" f
  where
    f mterm =
      mterm >>= \case
        Binary (Number x) (Number y) -> return . Number $ x `mod` y
        Binary _ _ -> undefined
        Unary _ -> undefined

negate' :: MonadError EvalError m => [Term] -> m Term
negate' = arrity 1 "negate" f
  where
    f mterm =
      mterm >>= \case
        Unary (Number x) -> return . Number $ negate x
        Unary _ -> undefined
        Binary _ _ -> undefined

signum' :: MonadError EvalError m => [Term] -> m Term
signum' = arrity 1 "signum" f
  where
    f mterm =
      mterm >>= \case
        Unary (Number x) -> return . Number $ signum x
        Unary _ -> undefined
        Binary _ _ -> undefined
