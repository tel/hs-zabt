{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zabt.Internal.Term where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Zabt.Arity
import Zabt.Internal.Index
import Zabt.Internal.Nameless

-- | An abstract @'Term' v f@ is an abstract binding tree of the shape described
-- by the pattern functor @f@ augmented with variables named by @v@. Equality is
-- alpha-equivalence. In particular, @'Term' v f@ is (morally) equivalent to the
-- fixed-point of the pattern-algebra 'Zabt.View.View' respecting the binding
-- properties of 'Zabt.View.VAbs and 'Zabt.View.VVar'.
data Term v f a
  = Term
    { free    :: Set v
    , project :: Nameless v f (Term v f) a
    }

-- | A 'Flat' 'Term' is one which is not immediately binding any variables.
type Flat v f = Term v f G

-- | Returns the free variables used within a given @Term@.
--
-- NOTE: We have to define a new function in order to not accidentally break
-- encapsulation. Just exporting @free@ direction would allow uses to manipulate
-- the Term value and break invariants (!).
freeVars :: Term v f a -> Set v
freeVars = free

embed :: (Ord v, Foldable f) => Nameless v f (Term v f) a -> Term v f a
embed nls = case nls of
  Free v -> Term (Set.singleton v) nls
  Bound i -> Term Set.empty nls
  Pattern f -> Term (foldMap free f) nls
  -- NOTE that embedding Abstraction here doesn't affect the free variables! That
  -- only occurs when embedding a View
  Abstraction v nls' -> Term (free nls') nls

instance (Show v, Show (Nameless v f (Term v f) a)) => Show (Term v f a) where
  showsPrec p t = showsPrec p (project t)

deriving instance (Eq v, Eq (f (Term v f G))) => Eq (Term v f G)
deriving instance (Eq v, Eq (f (Term v f (B a))), Eq (Term v f a)) => Eq (Term v f (B a))

deriving instance (Ord v, Ord (f (Term v f G))) => Ord (Term v f G)
deriving instance (Ord v, Ord (f (Term v f (B a))), Ord (Term v f a)) => Ord (Term v f (B a))

var :: (Foldable f, Ord v) => v -> Flat v f
var v = embed (Free v)

abstract :: forall v f a . (Functor f, Ord v) => v -> Term v f a -> Term v f a
abstract name = go zero where
  go :: forall a . Index -> Term v f a -> Term v f a
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

substitute :: forall v f a . (Foldable f, Functor f, Ord v) => v -> (Term v f a -> Term v f a)
substitute = substitute' . var

substitute' :: forall v f a . (Foldable f, Functor f, Ord v) => Flat v f -> (Term v f a -> Term v f a)
substitute' value = go zero where
  go :: forall a . Index -> Term v f a -> Term v f a
  go idx t = 
    case project t of
      Free v -> t
      Bound idx' 
        | idx == idx' -> value
        | otherwise -> t
      Abstraction v t' -> embed (Abstraction v (go (next idx) t'))
      Pattern f -> embed (Pattern (fmap (go idx) f))

-- | Substitute some free variables.
subst :: forall v f a . (Foldable f, Functor f, Ord v) => Map v (Flat v f) -> (Term v f a -> Term v f a)
subst ss = go where
  loose = Set.fromList (Map.keys ss)
  go :: forall a . Term v f a -> Term v f a
  go t
    | Set.null (Set.intersection loose (free t)) = t
    | otherwise =
      case project t of
        Free v -> case Map.lookup v ss of
          Nothing -> t
          Just value -> value
        Bound _ -> t
        Abstraction v t' -> embed (Abstraction v (go t'))
        Pattern f -> embed (Pattern (fmap go f))

-- | Substitute just one free variable.
subst1 :: forall v f a . (Foldable f, Functor f, Ord v) => (v, Flat v f) -> (Term v f a -> Term v f a)
subst1 (v, value) = subst (Map.singleton v value)
