{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Zabt.Internal.Term where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Zabt.Internal.Index
import Zabt.Internal.Nameless

-- | An abstract @'Term' v f@ is an abstract binding tree of the shape described
-- by the pattern functor @f@ augmented with variables named by @v@. Equality is
-- alpha-equivalence. In particular, @'Term' v f@ is (morally) equivalent to the
-- fixed-point of the pattern-algebra 'Zabt.View.View' respecting the binding
-- properties of 'Zabt.View.VAbs' and 'Zabt.View.VVar'.
data Term v f
  = Term
    { free    :: Set v
    , project :: Nameless v f (Term v f)
    }

deriving instance (Eq v, Eq (f (Term v f))) => Eq (Term v f)
deriving instance (Ord v, Ord (f (Term v f))) => Ord (Term v f)

instance (Show v, Show (Nameless v f (Term v f))) => Show (Term v f) where
  showsPrec p t = showsPrec p (project t)

-- | Returns the free variables used within a given @Term@.
--
-- NOTE: We have to define a new function in order to not accidentally break
-- encapsulation. Just exporting @free@ direction would allow uses to manipulate
-- the Term value and break invariants (!).
freeVars :: Term v f -> Set v
freeVars = free

embed :: (Ord v, Foldable f) => Nameless v f (Term v f) -> Term v f
embed nls = case nls of
  Free v -> Term (Set.singleton v) nls
  Bound i -> Term Set.empty nls
  Pattern f -> Term (foldMap free f) nls
  -- NOTE that embedding Abstraction here doesn't affect the free variables! That
  -- only occurs when embedding a View
  Abstraction v nls' -> Term (free nls') nls

var :: (Foldable f, Ord v) => v -> Term v f
var v = embed (Free v)

abstract :: (Foldable f, Functor f, Ord v) => v -> Term v f -> Term v f
abstract name = go zero where
  go idx t 
    | not (Set.member name (free t)) = t
    | otherwise = 
        Term (Set.delete name (free t)) $ case project t of
          Free v 
            | v == name -> Bound idx
            | otherwise -> Free v
          Bound{} -> project t
          Abstraction v t' -> Abstraction v (go (next idx) t')
          Pattern f -> Pattern (fmap (go idx) f)

substitute :: (Functor f, Foldable f, Ord v) => v -> (Term v f -> Term v f)
substitute = substitute' . var

substitute' :: (Functor f, Foldable f, Ord v) => Term v f -> (Term v f -> Term v f)
substitute' value = go zero where
  go idx t = 
    case project t of
      Free v -> t
      Bound idx' 
        | idx == idx' -> value
        | otherwise -> t
      Abstraction v t' -> embed (Abstraction v (go (next idx) t'))
      Pattern f -> embed (Pattern (fmap (go idx) f))

-- | Substitute some free variables.
subst :: (Functor f, Foldable f, Ord v) => (v -> Maybe (Term v f)) -> (Term v f -> Term v f)
subst ss = go where
  go t = case project t of
    Free v -> fromMaybe t (ss v)
    Bound _ -> t
    Abstraction v t' -> embed (Abstraction v (go t'))
    Pattern f -> embed (Pattern (fmap go f))

-- | Substitute some free variables from a finite map.
substMap :: (Functor f, Foldable f, Ord v) => Map v (Term v f) -> (Term v f -> Term v f)
substMap ss = subst (`Map.lookup` ss)

-- | Substitute just one free variable.
subst1 :: (Functor f, Foldable f, Ord v) => (v, Term v f) -> (Term v f -> Term v f)
subst1 (v, value) = subst (\v' -> if v == v' then Just value else Nothing)
