{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Test.Tasty
import Test.Tasty.Hspec
import Control.Monad (mapM)

import Zabt
import Zabt.Name
import Zabt.Internal.Nameless
import Zabt.Internal.Index
import Zabt.Internal.Term

import qualified STLC
import qualified Patterns 

main :: IO ()
main = do
  tests <- buildTests 
    [ ("Runner", runner)
    , ("Examples", baseExamples)
    , ("STLC", STLC.tests)
    , ("Bidirectional with Patterns", Patterns.tests)
    ]
  defaultMain tests

buildTests :: [(TestName, IO [TestTree])] -> IO TestTree
buildTests specs = do
  sets <- mapM (\(name, builder) -> testGroup name <$> builder) specs
  return (testGroup "All" sets)

runner :: IO [TestTree]
runner = testSpecs $
  it "equates unit" $ () == ()

baseExamples :: IO [TestTree]
baseExamples = testSpecs $ do

  describe "Var \"a\" :: Term Name []" $ do
    let ex = Var "a" :: Term Name []
    it "==  Var \"a\"" $ 
      ex == Var "a"
    it "/=  Var \"b\"" $ 
      ex /= Var "b"

  describe "Abs \"foo\" (Var \"foo\") :: Term Name []" $ do
    let ex = Abs "foo" (Var "foo") :: Term Name []
    it "==   Abs \"bar\" (Var \"bar\")" $ 
       ex == Abs "bar" (Var "bar")

  describe "unfold $ subst1 (\"bar\", Var \"foo\") (Abs \"foo\" (Pat [Var \"foo\", Var \"bar\"])) :: View Name [] (Term Name [])" $ do
    let ex = unfold $ subst1 ("bar", Var "foo") (Abs "foo" (Pat [Var "foo", Var "bar"])) :: View Name [] (Term Name [])
    it "==   VAbs (freshen \"foo\") (Pat [Var (freshen \"foo'\"),Var \"foo\"])" $
      ex == VAbs (freshen "foo") (Pat [Var (freshen "foo"), Var "foo"])

  describe "Abs \"foo\" (Var \"foo\") :: Term Name []" $ do
    let ex = Abs "foo" (Var "foo") :: Term Name []
    it "==   Abstraction \"foo\" (Bound 1) " $
      ex == embed (Abstraction (Scope "foo" (embed (Bound zero))))

  describe "Abs \"a\" (Pat [Var \"a\", Var \"b\"]) :: Term Name []" $ do
    let ex = Abs "a" (Pat [Var "a", Var "b"]) :: Term Name []
    it "==   Abstraction \"a\" (Pattern [Bound 1, Free \"b\"])" $
      ex == embed (Abstraction (Scope "a" (embed (Pattern [embed (Bound zero), embed (Free "b")]))))
