module Main where

import Data.Functor.Compose
import Control.Monad.Except (runExcept)
import Control.Applicative
import Data.Function (on)
import Text.Trifecta

-- | TODO: Add QuickCheck
import Test.Hspec

import Lib


yields :: Eq a => a -> Result a -> Bool
yields s (Success s') = s == s'
yields _ _ = False

parseFailed :: Result a -> Bool
parseFailed (Success _) = False
parseFailed _ = True

specParseYields :: String -> Term -> SpecWith ()
specParseYields s term =
    it ("parses " ++ s ++ " as " ++ show term) $
        parse s `shouldSatisfy` yields term

specParseFails :: String -> SpecWith ()
specParseFails s =
    it ("fails to parse " ++ s) $
        parse s `shouldSatisfy` parseFailed

specEvalYields :: Term -> Either EvalError Term -> SpecWith ()
specEvalYields term eterm =
    it ("evaluates " ++ show term ++ " as " ++ show eterm) $
        (runExcept . evalTerm) term `shouldBe` eterm
    
-- | TODO: Add unhappy parses
checkParse :: SpecWith ()
checkParse = describe "Test Parser" $
    mapM_ (uncurry specParseYields)
        [ ("1", Number 1)
        , ("True", Boolean True)
        , ("False", Boolean False)
        , ("\"Foobar\"", String "Foobar")
        , ("add", Symbol "add")
        , ("()", List Nil)
        , ("(1)", List (Number 1 :-: Nil))
        , ("(True)", List (Boolean True :-: Nil))
        , ("(1 2 3)", List (Number 1 :-: Number 2 :-: Number 3 :-: Nil))
        , ("(1 2 (True False))", List (Number 1 :-: Number 2 :-: List (Boolean True :-: Boolean False :-: Nil) :-: Nil))
        , ("(1 . 2)", DotList (Number 1 :-. Number 2))
        , ("(1 2 . 3)", DotList (Number 1 :-: Number 2 :-. Number 3))
        , ("(1 . (2 . (3 . 4)))", DotList (Number 1 :-. DotList (Number 2 :-. DotList (Number 3 :-. Number 4))))
        , ("'(1 2)", List (Symbol "quote" :-: List (Number 1 :-: Number 2 :-: Nil) :-: Nil))
        , ("(add 1 2)", List (Symbol "add" :-: Number 1 :-: Number 2 :-: Nil))
        , ("(lambda x (add 1 2))", List (Symbol "lambda" :-: Symbol "x" :-: List (Symbol "add" :-: Number 1 :-: Number 2 :-: Nil) :-: Nil))
        , ("(cons 1 2)", List (Symbol "cons" :-: Number 1 :-: Number 2 :-: Nil))
        , ("(cons 1 (cons 2 ()))", List (Symbol "cons" :-: Number 1 :-: List (Symbol "cons" :-: Number 2 :-: (List Nil :-: Nil)) :-: Nil))
        , ("(cons 1 '(2 3))", List (Symbol "cons" :-: Number 1 :-: List (Symbol "quote" :-: List (Number 2 :-: Number 3 :-: Nil) :-: Nil) :-: Nil))
        ]
   
-- | TODO: Add unhappy evaluations
checkEval :: SpecWith ()
checkEval = describe "Test Evaluation" $ do
  describe "Success" $
    mapM_ (uncurry specEvalYields) $
      getCompose $ Right <$> Compose
      [ (Number 1, Number 1)
      , (Boolean True, Boolean True)
      , (Boolean False, Boolean False)
      , (String "Foobar", String "Foobar")
      , (Symbol "add", Symbol "add")
      , (Symbol "lambda", Symbol "lambda")
      , (List Nil, List Nil)

      -- add
      , (List (Symbol "add" :-: Number 1 :-: Number 2 :-: Nil), Number 3)
      , (List (Symbol "add" :-: Number 1 :-: List (Symbol "add" :-: Number 2 :-: List (Symbol "add" :-: Number 3 :-: Number 4 :-: Nil) :-: Nil) :-: Nil), Number 10)
      -- quote
      , (List (Symbol "quote" :-: List (Number 1 :-: Number 2 :-: Nil) :-: Nil), List (Number 1 :-: Number 2 :-: Nil))
      , (List (Symbol "quote" :-: DotList (Number 1 :-. Number 2) :-: Nil), DotList (Number 1 :-. Number 2))
      -- atom
      , (List (Symbol "atom?" :-: Number 1 :-: Nil), Boolean True)
      , (List (Symbol "atom?" :-: DotList Nil :-: Nil), Boolean False)
      , (List (Symbol "atom?" :-: List (Symbol "quote" :-: List (Number 1 :-: Nil) :-: Nil) :-: Nil) , Boolean False)
      -- eq
      , (List (Symbol "eq?" :-: Number 1 :-: Number 1 :-: Nil), Boolean True)
      , (List (Symbol "eq?" :-: Number 1 :-: Number 2 :-: Nil), Boolean False)
      , (List (Symbol "eq?" :-: Number 3 :-: List (Symbol "add" :-: Number 1 :-: Number 2 :-: Nil) :-: Nil), Boolean True)
      -- cons
      , (List (Symbol "cons" :-: Number 1 :-: List (Symbol "quote" :-: List (Number 2 :-: Number 3 :-: Nil) :-: Nil) :-: Nil), List (Number 1 :-: Number 2 :-: Number 3 :-: Nil))
      , (List (Symbol "cons" :-: Number 1 :-: Number 2 :-: Nil), DotList (Number 1 :-. Number 2))
      -- car
      , (List (Symbol "car" :-: List (Symbol "quote" :-: List (Number 1 :-: Number 2 :-: Nil) :-: Nil) :-: Nil), Number 1)
      , (List (Symbol "car" :-: List (Symbol "quote" :-: DotList (Number 1 :-. Number 2) :-: Nil) :-: Nil), Number 1)
      -- cdr
      , (List (Symbol "cdr" :-: List (Symbol "quote" :-: List (Number 1 :-: Number 2 :-: Nil) :-: Nil) :-: Nil), List (Number 2 :-: Nil))
      --, (List (Symbol "cdr" :-: List (Symbol "quote" :-: DotList (Number 1 :-. Number 2) :-: Nil) :-: Nil), Number 2)
      ]
  describe "Failure" $
    mapM_ (uncurry specEvalYields) $
      getCompose $ Left <$> Compose
      [ (List (Number 1 :-: Nil), ObjectNotApplicable (Number 1))
      -- add -- TODO: figure out how to label the error thrown from `asInteger` with the actual func called
      , (List (Symbol "add" :-: Number 1 :-: Boolean True :-: Nil), TypeError "asInteger" (Boolean True))
      -- quote
      -- atom
      -- eq
      -- cons
      , (List (Symbol "cons" :-: List (Number 1 :-: Number 2 :-: Nil) :-: Number 3 :-: Nil), ObjectNotApplicable (Number 1))
      -- car
      , (List (Symbol "car" :-: List (Number 1 :-: Number 2 :-: Nil) :-: Nil), ObjectNotApplicable (Number 1))
      -- cdr
      , (List (Symbol "cdr" :-: List (Number 1 :-: Number 2 :-: Nil) :-: Nil), ObjectNotApplicable (Number 1))
      -- List
      , (List (Boolean True :-: Number 1 :-: Nil), ObjectNotApplicable (Boolean True))
      ]

main :: IO ()
main = hspec $ do
    checkParse
    checkEval
