{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module STLC where

import Test.Tasty
import Test.Tasty.Hspec

import Zabt
import Zabt.Name

data Alg x 
  = App x x
  | Lam x
  deriving (Eq, Ord, Show, Functor, Foldable)

type Exp = Term Name Alg

pattern (:$) :: Exp -> Exp -> Exp
pattern (:$) f x = Pat (App f x) 

pattern (:\) :: Name -> Exp -> Exp
pattern (:\) v e = Pat (Lam (Abs v e))

infixr 0 :\
infixr 1 :$

ex1, ex2, ex3 :: Exp

ex1 = Var "foo"
ex2 = "foo" :\ Var "foo"
ex3 = ex2 :$ ex1

whnf :: Exp -> Exp
whnf x = case x of
  Var _ -> x
  _ :\ _ -> x
  (v :\ e) :$ x -> subst1 (v, x) e

--
--
-- 

tests :: IO [TestTree]
tests = testSpecs $ do

  describe "whnf" $ do
    it "has [[foo]] ---> [[foo]]" (ex1 == whnf ex1)
    it "has [[(\\x -> x) foo]] ---> [[foo]]" (ex1 == whnf ex3)

  describe "capture avoidance" $ do
    it "has [[(\\x -> \\y -> x) y]] ---> [[\\z -> y]]" $ 
      let f = "x" :\ "y" :\ Var "x"
          a = f :$ Var "y"
          b = "z" :\ Var "y"
      in whnf a == b
