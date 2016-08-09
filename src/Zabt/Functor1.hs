{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Zabt.Functor1 where

class Functor1 (h :: (k -> *) -> (k -> *)) where
  hmap :: (forall x . f x -> g x) -> h f a -> h g a
