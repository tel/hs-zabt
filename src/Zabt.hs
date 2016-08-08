{-# LANGUAGE PatternSynonyms #-}

-- | Abstract binding trees with a nameless internal representation.
module Zabt ( 

    Term

  , pattern Var
  , pattern Lam
  , pattern Pat

  , subst
  , subst1
  , freeVars

  , View (..)
  , fold
  , unfold

  , Freshen (..)




) where

import Zabt.Internal
