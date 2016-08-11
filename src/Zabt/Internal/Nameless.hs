{-# LANGUAGE FlexibleContexts #-}

module Zabt.Internal.Nameless where

import GHC.Show (showSpace)

import Zabt.Internal.Index

data Nameless v f x
  = Free !v
  | Bound !Index
  | Pattern (f x)
  | Abstraction !v x
  deriving (Eq, Ord)

instance (Show v, Show (f x), Show x) => Show (Nameless v f x) where
  showsPrec p (Free v) = showsPrec 11 v
  showsPrec p (Bound i) = showString "'" . showsPrec 11 i
  showsPrec p (Pattern f) = showsPrec p f
  showsPrec p (Abstraction v t) = showParen (p >= 11) $
      showString "λ" 
    . showSpace 
    . showsPrec 11 t
