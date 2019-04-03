module Main where

import Control.Applicative
import Text.Trifecta
import Test.Hspec
import Data.Function (on)
import Lib


yields :: Eq a => a -> Result a -> Bool
yields s (Success s') = s == s'
yields _ _ = False

parseFailed :: Result a -> Bool
parseFailed (Success _) = False
parseFailed _ = True

specParseYields :: String -> Term -> SpecWith ()
specParseYields s expr =
    it ("parses " ++ s ++ " as " ++ show expr) $
        parse s `shouldSatisfy` yields expr

specParseFails :: String -> SpecWith ()
specParseFails s =
    it ("fails to parse " ++ s) $
        parse s `shouldSatisfy` parseFailed

checkParse :: IO ()
checkParse = hspec . describe "Test Parser" $
    mapM_ (uncurry specParseYields)
        [ ("1", Number 1)
        , ("True", Boolean True)
        , ("False", Boolean False)
        , ("\"Foobar\"", String "Foobar")
        , ("add", Atom "add")
        , ("lambda", Atom "lambda")
        , ("()", Nil)
        , ("(1)", Cons (Number 1) Nil)
        , ("(1 2 3)", Cons (Number 1) (Cons (Number 2) (Cons (Number 3) Nil)))
        , ("(1 . 2 . 3)", Cons (Number 1) (Cons (Number 2) (Cons (Number 3) Nil)))
        , ("(1 2 (True False))", Cons (Number 1) (Cons (Number 2) (Cons (Cons (Boolean True) (Cons (Boolean False) Nil)) Nil)))
        , ("(add (1 2))", add12)
        , ("(lambda x (add 1 2))", Cons (Atom "lambda") (Cons (Atom "x") (Cons (Cons (Atom "add") (Cons (Number 1) (Cons (Number 2) Nil))) Nil)))
        ]

add = Atom "add"
add12 = Cons add (Cons (Cons (Number 1) (Cons (Number 2) Nil)) Nil)
   
checkEval :: IO ()
checkEval = hspec . describe "Test Evaluation" $ undefined

main :: IO ()
main = checkParse
