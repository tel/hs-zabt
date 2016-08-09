{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Zabt.View where

import GHC.Show

import Zabt.Arity
import Zabt.Freshen
import Zabt.Internal.Nameless
import Zabt.Internal.Term
import Zabt.Visits

data View v (f :: (Arity -> *) -> *) (x :: Arity -> *) (a :: Arity) where
  VVar :: !v -> View v f x G
  VPat :: f x -> View v f x G
  VAbs :: !v -> x a -> View v f x (B a)

-- NOTE: These instances are gnarly because we have to manually tell GHC how to do
-- induction over arities!

instance (Show v, Show (x G), Show (f x)) => Show (View v f x G) where
  showsPrec p (VVar v) = showParen (p >= 11) $ showString "VVar " . showsPrec 11 v
  showsPrec p (VPat f) = showParen (p >= 11) $ showString "VPat " . showsPrec 11 f

instance (Show v, Show (x a), Show (f x)) => Show (View v f x (B a)) where
  showsPrec p (VAbs v t) = showParen (p >= 11) $
      showString "VAbs " 
    . showsPrec 11 v 
    . showSpace 
    . showsPrec 11 t

-- | /Alpha/-equivalence
instance (Eq v, Eq (f x)) => Eq (View v f x G) where
  VVar va == VVar vb = va == vb
  VPat fa == VPat fb = fa == fb

-- | /Alpha/-equivalence
instance (Eq (x a), Eq (f x)) => Eq (View v f x (B a)) where
  VAbs _ ta == VAbs _ tb = ta == tb

instance (Ord v, Ord (f x)) => Ord (View v f x G) where
  VVar va `compare` VVar vb = va `compare` vb
  VPat fa `compare` VPat fb = fa `compare` fb

instance (Ord v, Ord (x a), Ord (f x)) => Ord (View v f x (B a)) where
  VAbs _ ta `compare` VAbs _ tb = ta `compare` tb

-- | @'Var' v@ creates and matches a 'Term' value corresponding to a free
-- variable.
pattern Var a <- (unfold -> VVar a) where
  Var a = fold (VVar a)

-- | @'Abs' v t@ creates and matches a 'Term' value where the free variable @v@
-- has been abstracted over, becoming bound.
pattern Abs v t <- (unfold -> VAbs v t) where
  Abs v t = fold (VAbs v t)

-- | @'Pat' f@ creates and matches a 'Term' value built from a layer of the
-- pattern functor @f@.
pattern Pat f <- (unfold -> VPat f) where
  Pat f = fold (VPat f)

fold :: (Visits f, Ord v) => View v f (Term v f) a -> Term v f a
fold v = case v of
  VVar v -> var v
  VAbs v t -> embed (Abstraction v (abstract v t))
  VPat f -> embed (Pattern f)

unfold :: (Visits f, Ord v, Freshen v) => Term v f a -> View v f (Term v f) a
unfold t = 
  case project t of
    Free v -> VVar v
    Bound _idx -> error "naked bound variable, invariant broken!"
    Abstraction v t' -> 
      let v' = freshWrt (free t) v in
      VAbs v' (substitute v' t')
    Pattern f -> VPat f
