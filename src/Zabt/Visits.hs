{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Zabt.Visits ( 

    Visits (..)

) where

import Zabt.Arity

-- | A type @f :: Arity -> *@ instantiating @'Visits' f@ essentially is
-- 'Traversable', but we define a new typeclass to handle the difference in
-- types.
class Visits (t :: (Arity -> *) -> *) where
  visit :: Applicative f => (forall a. x a -> f (y a)) -> t x -> f (t y)

  vmap :: (forall a. x a -> y a) -> (t x -> t y)
  vmap f = getI . visit (I . f)

  vfoldMap :: Monoid m => (forall a. x a -> m) -> (t x -> m)
  vfoldMap f = getK . visit (K . f)

---

newtype I a = I { getI :: a } deriving Functor

instance Applicative I where
  pure = I
  I f <*> I a = I (f a)

newtype K a b = K { getK :: a } deriving Functor 

instance Monoid a => Applicative (K a) where
  pure _ = K mempty
  K a <*> K b = K (mappend a b)

