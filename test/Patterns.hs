{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

-- | From Neel Krishnaswami's "Abstract Binding Trees, an addendum"
--
-- <https://semantic-domain.blogspot.co.uk/2015/03/abstract-binding-trees-addendum.html>
module Patterns where

import Test.Tasty
import Test.Tasty.Hspec

import Zabt
import Zabt.Name
import Control.Monad (forM_, guard)

data Tp
  = Arrow Tp Tp
  | One
  | Prod Tp Tp
  | Sum Tp Tp
  deriving (Eq, Ord, Show)

data Pat 
  = PWild
  | PVar
  | PUnit
  | PPair Pat Pat
  | PInl Pat
  | PInr Pat
  deriving (Eq, Ord, Show)

data TmF x
  = TLam x
  | TApp x x
  | TAnnot Tp x
  | TUnit
  | TPair x x
  | TInl x
  | TInr x
  | TCase x [(Pat, x)]
  deriving (Eq, Ord, Show, Functor, Foldable)

pattern v :\ x = Pat (TLam (Abs v x))
pattern f :$ x = Pat (TApp f x)
pattern tm ::: tp = Pat (TAnnot tp tm)
pattern Unit = Pat TUnit
pattern a :* b = Pat (TPair a b)
pattern Inl a = Pat (TInl a)
pattern Inr a = Pat (TInr a)
pattern Case scr pats = Pat (TCase scr pats)

type Tm = Term Name TmF

type Ctx = [(Name, Tp)]

isCheck :: Tm -> Bool
isCheck (Pat x) = case x of
  TLam _ -> True
  TUnit -> True
  TPair _ _ -> True
  TInl _ -> True
  TInr _ -> True
  TCase _ _ -> True
  _ -> False
isCheck _ = False

isSynth :: Tm -> Bool
isSynth = not . isCheck

done :: Maybe ()
done = return ()

check :: Ctx -> Tm -> Tp -> Maybe ()
check ctx e tp = case (e, tp) of
  (x :\ e', Arrow tp1 tp') -> check ((x, tp1) : ctx) e' tp'
  (Unit, One) -> done
  (e :* e', Prod tp tp') -> check ctx e tp >> check ctx e' tp'
  (Inl e, Sum tp _) -> check ctx e tp
  (Inr e, Sum _ tp) -> check ctx e tp
  (Case e branches, tp) -> do
    tp' <- synth ctx e
    forM_ branches $ \(p, e) -> 
      checkBranch tp ctx [p] e [tp']
    done
  (body, _) | isSynth body -> do
    tp' <- synth ctx e
    guard (tp == tp')
  _ -> Nothing

synth :: Ctx -> Tm -> Maybe Tp
synth ctx e = case e of
  Var x -> lookup x ctx
  e ::: tp -> do 
    check ctx e tp
    return tp
  f :$ e -> do
    Arrow tp tp' <- synth ctx f
    check ctx e tp
    return tp'
  _ -> Nothing

checkBranch :: Tp -> Ctx -> [Pat] -> Tm -> [Tp] -> Maybe ()
checkBranch tpResult = go where
  go ctx ps e tps =
    case (ps, tps) of
      ([], []) -> check ctx e tpResult
      (p : ps', tp : tps') ->
        case (p, tp) of
          (PVar, _) -> case e of
            x :\ e' -> go ((x, tp) : ctx) ps' e' tps'
          (PWild, _) -> go ctx ps' e tps'
          (PUnit, One) -> go ctx ps' e tps'
          (PPair pl pr, Prod tpl tpr) -> go ctx (pl : pr : ps') e (tpl : tpr : tps')
          (PInl pl, Sum tpl _) -> go ctx (pl : ps') e (tpl : tps')
          (PInr pr, Sum _ tpr) -> go ctx (pr : ps') e (tpr : tps')
          _ -> Nothing

--
--
--

ok :: Maybe a -> Bool
ok Just{} = True
ok _ = False

tests :: IO [TestTree]
tests = testSpecs $ do

  describe "check" $ do
    it "Unit ::: One" $ ok $ check [] Unit One
    it "x : One |- x ::: One" $ ok $ check [("x", One)] (Var "x") One
    it "\\x -> Unit ::: Unit -> Unit" $ ok $ check [] ("x" :\ Unit) (Arrow One One)
    it "\\x -> x ::: Unit -> Unit" $ ok $ check [] ("x" :\ Var "x") (Arrow One One)
