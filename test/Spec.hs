{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Test.Tasty
import Test.Tasty.Hspec
import Control.Monad (mapM)

import Zabt
import Zabt.Internal.Nameless
import Zabt.Internal.Index
import Zabt.Internal.Term

main :: IO ()
main = do
  tests <- buildTests 
    [ ("Runner", runner)
    , ("Examples", baseExamples)
    ]
  defaultMain tests

buildTests :: [(TestName, IO [TestTree])] -> IO TestTree
buildTests specs = do
  sets <- mapM (\(name, builder) -> testGroup name <$> builder) specs
  return (testGroup "All" sets)

runner :: IO [TestTree]
runner = testSpecs $
  it "equates unit" $ () == ()

-- We don't define this for people since it's a matter of style, but it's
-- enough for testing purposes.
instance Freshen String where
  freshen s = s ++ "'"

-- A simple pattern for us
data Pair x = Pair (x A0) (x A0)

deriving instance Eq (x A0) => Eq (Pair x)
deriving instance Show (x A0) => Show (Pair x)

instance Visits Pair where
  visit phi (Pair a b) = Pair <$> phi a <*> phi b

baseExamples :: IO [TestTree]
baseExamples = testSpecs $ do

  describe "Abs 'foo' (Var 'foo')" $ do
    let ex = Abs "foo" (Var "foo") :: Term String Pair A1
    it "equals Abs 'bar' (Var 'bar') at Term String Pair A1" $ 
       ex == Abs "bar" (Var "bar")

  describe "unfold $ subst1 ('bar', Var 'foo') (Abs 'foo' (Pat [Var 'foo', Var 'bar']))" $ do
    let ex = unfold $ subst1 ("bar", Var "foo") (Abs "foo" (Pat (Pair (Var "foo") (Var "bar")))) :: View String Pair (Term String Pair) A1
    it "equals VAbs 'foo'' (Pat [Var 'foo'',Var 'foo']) at View String Pair (Term String Pair) A1" $
      ex == VAbs "foo'" (Pat (Pair (Var "foo'") (Var "foo")))

  describe "Abs 'foo' (Var 'foo')" $ do
    let ex = Abs "foo" (Var "foo") :: Term String Pair A1
    it "equals Abst 'foo' (B 1) at Term String Pair A1" $
      ex == embed (Abstraction "foo" (embed (Bound zero)))

  describe "Abs 'a' (Pat [Var 'a', Var 'b'])" $ do
    let ex = Abs "a" (Pat (Pair (Var "a") (Var "b"))) :: Term String Pair A1
    it "equals Abst 'a' (Branch [B 1,F 'b']) at Term String Pair A1" $
      ex == embed (Abstraction "a" (embed (Pattern (Pair (embed (Bound zero)) (embed (Free "b"))))))

