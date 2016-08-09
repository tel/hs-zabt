{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Zabt.Internal.Nameless where

import GHC.Show (showSpace)

import Zabt.Arity
import Zabt.Functor1
import Zabt.Internal.Index

data Nameless v (f :: * -> *) (x :: Arity -> *) (a :: Arity) where
  Free :: !v -> Nameless v f x G
  Bound :: !Index -> Nameless v f x G
  Pattern :: f (x a) -> Nameless v f x a
  Abstraction :: !v -> x a -> Nameless v f x (B a)

instance Functor f => Functor1 (Nameless v f) where
  hmap phi x = case x of
    Free v          -> Free v
    Bound i         -> Bound i
    Pattern f       -> Pattern (fmap phi f)
    Abstraction v x -> Abstraction v (phi x)

-- NOTE: These instances are gnarly because we have to manually tell GHC how to do
-- induction over arities!

showsPrecPattern :: Show (f (x a)) => Int -> f (x a) -> ShowS
showsPrecPattern p f = showParen (p >= 11) $ showString "Pattern " . showsPrec 11 f

instance (Show v, Show (x G), Show (f (x G))) => Show (Nameless v f x G) where
  showsPrec p (Free v) = showParen (p >= 11) $ showString "Free " . showsPrec 11 v
  showsPrec p (Bound i) = showParen (p >= 11) $ showString "Bound " . showsPrec 11 i
  showsPrec p (Pattern f) = showsPrecPattern p f

instance (Show v, Show (x a), Show (f (x (B a)))) => Show (Nameless v f x (B a)) where
  showsPrec p (Abstraction v t) = showParen (p >= 11) $
      showString "Abstraction " 
    . showsPrec 11 v 
    . showSpace 
    . showsPrec 11 t

  showsPrec p (Pattern f) = showsPrecPattern p f

-- | /Alpha/-equivalence
instance (Eq v, Eq (f (x G))) => Eq (Nameless v f x G) where
  Free va == Free vb = va == vb
  Bound ixa == Bound ixb = ixa == ixb
  Pattern fa == Pattern fb = fa == fb

-- | /Alpha/-equivalence
instance (Eq (x a), Eq (f (x (B a)))) => Eq (Nameless v f x (B a)) where
  Abstraction _ ta == Abstraction _ tb = ta == tb
  Pattern fa == Pattern fb = fa == fb

instance (Ord v, Ord (f (x G))) => Ord (Nameless v f x G) where
  Free va `compare` Free vb = va `compare` vb
  Bound ixa `compare` Bound ixb = ixa `compare` ixb
  Pattern fa `compare` Pattern fb = fa `compare` fb

instance (Ord v, Ord (x a), Ord (f (x (B a)))) => Ord (Nameless v f x (B a)) where
  Abstraction _ ta `compare` Abstraction _ tb = ta `compare` tb
  Pattern fa `compare` Pattern fb = fa `compare` fb
