{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Arities are type-level naturals used to describe the number of variables that
-- must be immediately bound at a Term.

module Zabt.Arity where

import GHC.Exts

data Arity = G | B Arity

type G = 'G
type B a = 'B a

-- Some useful shortcut arities.

type A0 = G
type A1 = B G
type A2 = B A1
type A3 = B A2

-- | The 'DownTo' type family makes it easier to declare type constraints over
-- families of arities.
type family DownTo (c :: * -> Constraint) (x :: Arity -> *) (a :: Arity) :: Constraint where
  DownTo c x G = c (x G)
  DownTo c x (B a) = (c (x (B a)), DownTo c x a)
