{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Lib where

import Data.Text (Text)

import Control.Monad.Except
import Control.Monad.Reader


import Evaluator
import Evaluator.Types
import Parser

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
  
