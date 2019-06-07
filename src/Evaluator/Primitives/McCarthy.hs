{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Evaluator.Primitives.McCarthy where

import Control.Monad.Except
import Control.Monad.State

import Evaluator.Types

quote :: (MonadEnv m, MonadState EvalEnv m, MonadError EvalError m) => [Term] -> m Term
quote = arrity 1 "quote?" f
  where
    f mterm =
      mterm >>= \case
        Unary xs -> return xs
        Binary _ _ -> undefined

-- | TODO: cond needs to be simplified drastically
cond :: (MonadEnv m, MonadState EvalEnv m, MonadError EvalError m) => (Term -> m Term) -> [Term] -> m Term
cond f [x] =
  case x of
    List [Boolean p, e] -> if p then f e else throwError UnspecifiedReturn
    List [Boolean p] -> f (Boolean p)
    List [term] -> f term
    List [Number _ , e]  -> f e
    DotList ([], pe) -> return pe
    _ -> throwError IllFormedSyntax
cond f (x:xs) =
  case x of
    List [Boolean pe] -> if pe then return (Boolean pe) else cond f xs
    List [Boolean p, e] -> if p then f e else cond f xs
    List [Number _ , e] -> f e
    List [] -> throwError UnspecifiedReturn
    List _ -> throwError IllFormedSyntax
    DotList ([Boolean p], e) -> if p then f e else cond f xs
    DotList ([], Boolean pe) -> if pe then return (Boolean pe) else cond f xs
    _ -> throwError IllFormedSyntax
cond _ _ = throwError IllFormedSyntax

eq :: MonadError EvalError m => [Term] -> m Term
eq = arrity 2 "eq?" f
  where
    f mterm = mterm >>= \case
      Binary t1 t2 -> return . Boolean $ t1 == t2
      Unary _ -> undefined

cons :: MonadError EvalError m => [Term] -> m Term
cons = arrity 2 "cons" f
  where
    f terms = terms >>= \case
      (Binary x (List xs) )   -> return $ List (x:xs)
      (Binary y (DotList (xs, x))) -> return $ DotList (y:xs, x)
      (Binary x y)            -> return $ DotList (pure x, y)
      Unary _ -> undefined

car :: MonadError EvalError m => [Term] -> m Term
car = arrity 1 "car" f
  where
    f term = term >>= \case
      Unary (List (x:_))    -> return x
      Unary (DotList ([], x)) -> return x
      Unary (DotList (x, _)) -> return $ head x
      Unary term'               -> throwError $ TypeError "car" term'
      Binary _ _ -> undefined
       
cdr :: MonadError EvalError m => [Term] -> m Term
cdr = arrity 1 "cdr" f
  where
    f term = term >>= \case
      Unary (DotList ([x], y)) -> return y
      Unary (DotList (xs, x)) -> return . DotList $ (tail xs, x)
      Unary (List (_:xs))   -> return . List $ xs
      Unary term' -> throwError $ TypeError "cdr" term'
      Binary _ _ -> undefined

atom :: MonadError EvalError m => [Term] -> m Term
atom = arrity 1 "atom?" f
  where
    f mterm =
      mterm >>= \case
        Unary (DotList xs) -> throwError $ NotAProperList (DotList xs)
        Unary (List _)     -> return     $ Boolean False
        Unary _            -> return     $ Boolean True
        Binary _ _ -> undefined

verifyArgs :: [Term] -> Either EvalError [String]
verifyArgs = traverse f
  where
    f (Symbol x) = Right x
    f _ = Left IllFormedSyntax

-- | TODO: Implement dat lambda
lambda :: MonadError EvalError m => [Term] -> m Term
lambda = arrity 1 "lambda" f
  where
    f mterm = mterm >>= \case
      Binary (List args) body -> undefined 
        --case verifyArgs args of
        --  Left err -> throwError err
        --  Right xs -> return $ Func xs body
      Binary _ _ -> throwError IllFormedSyntax
      Unary term -> undefined

apply :: (MonadError EvalError m, MonadEnv m) => (Term -> m Term) -> [Term] -> m Term
apply f = arrity 2 "eval" g
  where
    g mterm = mterm >>= \case
      Binary (Func vars body) (List values) -> undefined
        -- either throwError f $ substituteAll vars values body
      Binary _ atomic -> throwError $ TypeError "eval" atomic
      Unary _ -> undefined

type Lambda = Term
type Param = Term
type Value = Term
type Params = [Term]
type Values = [Term]

applyLambda :: MonadError EvalError m => (Term -> m Term) -> [Term] -> m Term
applyLambda f (Func params body:values) =
  case body of
    -- | TODO: Same param/value verification needed for List as Symbol
    -- | TODO: Symbol Body needs to not be evaluated until application. Where is this happening?
    List _ -> liftEither (substituteAll params values body) >>= f
    Symbol _ | null params -> f body
    Symbol _ | length params <= length values ->
      if head params == body
      then f (head values)
      else f body
    Symbol _ | length params > length values -> throwError (WrongArrity "lambda" (length params) (length values))
    _ | length values >= length params -> f body
    _  -> throwError $ WrongArrity "lambda" (length params) (length values)
applyLambda f terms = f $ List terms

substituteAll :: Params -> Values -> Term -> Either EvalError Term
substituteAll params values (List body) = 
  let paramMap = zip params values
  in Right $ foldr (\(param, term) (List xs) -> substituteValue param term xs) (List body) paramMap
substituteAll _ _ atomic = Left $ TypeError "apply" atomic 

substituteValue :: Param -> Value -> [Term] -> Term
substituteValue param value body = List $ fmap f body
  where
    f sym@(Symbol _) = if sym == param then value else sym
    f (List xs) = List $ fmap f xs
    f (DotList (xs, x)) = DotList (f <$> xs, f x)
    f t = t

define :: (MonadEnv m, MonadError EvalError m) => (Term -> m Term) -> [Term] -> m Term
define f = arrity 2 "define" g
  where
    g mterm = mterm >>= \case
      Binary (Symbol var) val -> do
        val' <- f val
        putVar var val'
      _ -> throwError IllFormedSyntax

  
