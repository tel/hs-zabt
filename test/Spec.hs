{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.Tasty
import Test.Tasty.Hspec
import Control.Monad (mapM)
import Zabt.Internal

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

baseExamples :: IO [TestTree]
baseExamples = testSpecs $ do

  describe "Lam 'foo' (Var 'foo')" $ do
    let ex = Lam "foo" (Var "foo") :: Term String []
    it "equals Lam 'bar' (Var 'bar') at Term String []" $ 
       ex == Lam "bar" (Var "bar")

  describe "unfold $ subst1 ('bar', Var 'foo') (Lam 'foo' (Pat [Var 'foo', Var 'bar']))" $ do
    let ex = unfold $ subst1 ("bar", Var "foo") (Lam "foo" (Pat [Var "foo", Var "bar"])) :: View String [] (Term String [])
    it "equals VLam 'foo'' (Pat [Var 'foo'',Var 'foo']) at View String [] (Term String [])" $ 
      ex == VLam "foo'" (Pat [Var "foo'",Var "foo"])

  describe "Lam 'foo' (Var 'foo')" $ do
    let ex = Lam "foo" (Var "foo") :: Term String []
    it "equals Abst 'foo' (B 1) at Term String []" $
      ex == close (Abst "foo" (close (B 1)))

  describe "Lam 'a' (Pat [Var 'a', Var 'b'])" $ do
    let ex = Lam "a" (Pat [Var "a", Var "b"]) :: Term String []
    it "equals Abst 'a' (Branch [B 1,F 'b']) at Term String []" $ 
      ex == close (Abst "a" (close (Branch [close (B 1), close (F "b")])))

