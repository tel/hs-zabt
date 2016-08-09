{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Zabt.Internal.Nameless where

import GHC.Show (showSpace)

import Zabt.Arity
import Zabt.Internal.Index

data Nameless v (f :: (Arity -> *) -> *) (x :: Arity -> *) (a :: Arity) where
  Free :: !v -> Nameless v f x G
  Bound :: !Index -> Nameless v f x G
  Pattern :: f x -> Nameless v f x G
  Abstraction :: !v -> x a -> Nameless v f x (B a)

-- NOTE: These instances are gnarly because we have to manually tell GHC how to do
-- induction over arities!

instance (Show v, Show (x G), Show (f x)) => Show (Nameless v f x G) where
  showsPrec p (Free v) = showsPrec 11 v
  showsPrec p (Bound i) = showString "'" . showsPrec 11 i
  showsPrec p (Pattern f) = showsPrec p f

instance (Show v, Show (x a), Show (f x)) => Show (Nameless v f x (B a)) where
  showsPrec p (Abstraction v t) = showParen (p >= 11) $
      showString "λ" 
    . showSpace 
    . showsPrec 11 t

-- | /Alpha/-equivalence
instance (Eq v, Eq (f x)) => Eq (Nameless v f x G) where
  Free va == Free vb = va == vb
  Bound ixa == Bound ixb = ixa == ixb
  Pattern fa == Pattern fb = fa == fb
  _ == _ = False

-- | /Alpha/-equivalence
instance (Eq (x a), Eq (f x)) => Eq (Nameless v f x (B a)) where
  Abstraction _ ta == Abstraction _ tb = ta == tb
  _ == _ = False

instance (Ord v, Ord (f x)) => Ord (Nameless v f x G) where
  Free va `compare` Free vb = va `compare` vb
  Bound ixa `compare` Bound ixb = ixa `compare` ixb
  Pattern fa `compare` Pattern fb = fa `compare` fb

instance (Ord v, Ord (x a), Ord (f x)) => Ord (Nameless v f x (B a)) where
  Abstraction _ ta `compare` Abstraction _ tb = ta `compare` tb
