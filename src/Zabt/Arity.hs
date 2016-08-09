{-# LANGUAGE DataKinds #-}

-- | Arities are type-level naturals used to describe the number of variables that
-- must be immediately bound at a Term.

module Zabt.Arity where

data Arity = G | B Arity

type G = 'G
type B a = 'B a

-- Some useful shortcut arities.

type A0 = G
type A1 = B G
type A2 = B A1
type A3 = B A2
