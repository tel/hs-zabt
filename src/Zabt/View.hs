{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Zabt.View where

import GHC.Show

import Zabt.Freshen
import Zabt.Internal.Nameless
import Zabt.Internal.Term

data View v f x
  = VVar !v
  | VPat (f x)
  | VAbs !v x
  deriving (Eq, Ord)

instance (Show v, Show x, Show (f x)) => Show (View v f x) where
  showsPrec p (VVar v) = showParen (p >= 11) $ showString "VVar " . showsPrec 11 v
  showsPrec p (VPat f) = showParen (p >= 11) $ showString "VPat " . showsPrec 11 f
  showsPrec p (VAbs v t) = showParen (p >= 11) $
      showString "VAbs " 
    . showsPrec 11 v 
    . showSpace 
    . showsPrec 11 t

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

fold :: (Foldable f, Functor f, Ord v) => View v f (Term v f) -> Term v f
fold v = case v of
  VVar v -> var v
  VAbs v t -> embed (Abstraction (Scope v (abstract v t)))
  VPat f -> embed (Pattern f)

unfold :: (Foldable f, Functor f, Ord v, Freshen v) => Term v f -> View v f (Term v f)
unfold t = 
  case project t of
    Free v -> VVar v
    Bound _idx -> error "naked bound variable, invariant broken!"
    Abstraction (Scope v t') -> 
      let v' = freshWrt (free t) v in
      VAbs v' (substitute v' t')
    Pattern f -> VPat f
