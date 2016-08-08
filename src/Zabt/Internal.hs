{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Internal module, expert use only. Using this module allows you to easily
-- break ABT invariants and generally have a terrible, no good, really bad time.
module Zabt.Internal where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- | A type which can be freshened has an operation which attempts to find a
-- unique version of its input. The principal thing that must hold is that
-- `freshen n /= n`. It's not necessary that `freshen n` be totally fresh with
-- respect to a context---that's too much to ask of a value---but it is
-- necessary that `freshen` *eventually* produces a fresh value.
class Eq v => Freshen v where
  freshen :: v -> v

instance Freshen Int where
  freshen n = n + 1

freshPred :: Freshen v => (v -> Bool) -> (v -> v)
freshPred used = go where
  go v = if used v then go (freshen v) else v

freshSet :: (Ord v, Freshen v) => Set v -> (v -> v)
freshSet used = freshPred (flip Set.member used)

-- | The Nameless algebra describes one layer of a nameless representation of a
-- binding tree parameterzed by another pattern algebra, f, which does not make
-- reference to variable binding.
data Nameless v f x
  = F v
  | B Int
  | Abst v x
  | Branch (f x)
  deriving (Show, Functor)

instance (Eq v, Eq x, Eq (f x)) => Eq (Nameless v f x) where
  F va == F vb = va == vb
  B ixa == B ixb = ixa == ixb
  Abst _ ta == Abst _ tb = ta == tb
  Branch fa == Branch fb = fa == fb

instance (Ord v, Ord x, Ord (f x)) => Ord (Nameless v f x) where
  F va `compare` F vb = va `compare` vb
  B ixa `compare` B ixb = ixa `compare` ixb
  Abst _ ta `compare` Abst _ tb = ta `compare` tb
  Branch fa `compare` Branch fb = fa `compare` fb

-- | An abstract @Term v f@ is an abstract binding tree of the shape described
-- by the pattern functor @f@ augmented with variables named by @v@. Equality is
-- alpha-equivalence. In particular, @Term v f@ is (morally) equivalent to the
-- fixed-point of the pattern-algebra @View@ respecting the binding
-- properties of @VLam@ and @VVar@.
data Term v f
  = Term
    { free :: Set v
    , open :: Nameless v f (Term v f)
    }

close :: (Ord v, Foldable f) => Nameless v f (Term v f) -> Term v f
close nls = case nls of
  F v -> Term { free = Set.singleton v, open = nls }
  B i -> Term { free = Set.empty, open = nls }
  Abst v nls' -> nls' { open = Abst v nls' }
  Branch f -> Term { free = foldMap free f, open = Branch f }

instance (Show v, Show (Nameless v f (Term v f))) => Show (Term v f) where
  showsPrec p t = showsPrec p (open t)

deriving instance (Eq v, Eq (f (Term v f))) => Eq (Term v f)
deriving instance (Ord v, Ord (f (Term v f))) => Ord (Term v f)

-- | A concrete view of the top "layer" of a @Term@ ABT. This top layer may be
-- an abstraction, a raw variable, or a branch of the pattern functor @f@.
-- Normally @View@ is wrapping a @Term@ value, the abstract "remainder" of the
-- tree.
data View v f x
  = VVar v
  | VLam v x
  | VPat (f x)
  deriving (Functor, Show, Eq)

-- | @Var v@ creates and matches a @Term@ value corresponding to a free
-- variable.
pattern Var a <- (unfold -> VVar a) where
  Var a = fold (VVar a)

-- | @Lam v t@ creates and matches a @Term@ value where the free variable @v@
-- has been abstracted over, becoming bound.
pattern Lam v t <- (unfold -> VLam v t) where
  Lam v t = fold (VLam v t)

-- | @Pat f@ creates and matches a @Term@ value built from a layer of the
-- pattern functor @f@.
pattern Pat f <- (unfold -> VPat f) where
  Pat f = fold (VPat f)

-- | Construct a @Term@ from one layer of a @View@.
fold :: (Functor f, Foldable f, Ord v) => View v f (Term v f) -> Term v f
fold v = case v of
  VVar v -> _var v
  VLam v t -> let tm = _abstr v t in tm { open = Abst v tm }
  VPat f -> Term { free = foldMap free f, open = Branch f }

-- | Strip the top layer of a @View@ off of a @Term@.
unfold :: (Functor f, Foldable f, Ord v, Freshen v) => Term v f -> View v f (Term v f)
unfold t@(Term { free = fvs, open = nls }) = 
  case nls of
    F v -> VVar v
    B _idx -> error "naked bound variable, invariant broken!"
    Abst v t' -> 
      let v' = freshSet fvs v in 
      VLam v' (_subst (1, _var v') t')

_var :: v -> Term v f
_var v = Term { free = Set.singleton v, open = F v }

_abstr :: (Functor f, Ord v) => v -> Term v f -> Term v f
_abstr name = go 1 where 
  go idx t@(Term { free = fvs, open = nls })
    | Set.member name fvs =
      Term { free = Set.delete name fvs
           , open = case nls of
               F v | v == name -> B idx
                   | otherwise -> F v
               B i -> nls
               Abst v t' -> open (go (idx + 1) t')
               Branch f -> Branch (fmap (go idx) f)
          }
    | otherwise = t

_subst :: (Foldable f, Functor f, Ord v) => (Int, Term v f) -> (Term v f -> Term v f)
_subst (idx, value) = go idx where
  go idx t@(Term { free = fvs, open = nls }) = 
    case nls of
      F v -> t
      B idx' | idx == idx' -> value
             | otherwise -> t
      Abst v t' -> 
        let res = go (idx + 1) t' in 
        res { open = Abst v res }
      Branch f -> 
        let res = fmap (go idx) f in 
        Term { free = foldMap free res, open = Branch res }

-- | Returns the free variables used within a given @Term@.
--
-- NOTE: We have to define a new function in order to not accidentally break
-- encapsulation. Just exporting @free@ direction would allow uses to manipulate
-- the Term value and break invariants (!).
freeVars :: Term v f -> Set v
freeVars = free

-- | Substitute some free variables.
subst :: (Foldable f, Functor f, Ord v) => Map v (Term v f) -> (Term v f -> Term v f)
subst ss = go where
  loose = Set.fromList (Map.keys ss)
  go t
    | Set.null (Set.intersection loose (free t)) = t
    | otherwise =
      case open t of
        F v -> case Map.lookup v ss of 
          Nothing -> t 
          Just value -> value
        B _ -> t
        Abst v t' -> 
          let res = go t' in 
          res { open = Abst v res }
        Branch f -> 
          let res = fmap go f in
          Term { free = foldMap free res, open = Branch res }

-- | Substitute just one free variable.
subst1 (v, value) = subst (Map.singleton v value)
