{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Abstract binding trees with a nameless internal representation.
module Zabt ( 

  -- * Abstract binding tree terms
  -- $intro

    Term
  , Flat

  -- * Constructing terms with 'View' patterns
  -- $patterns

  , pattern Var
  , pattern Abs
  , pattern Pat

  -- ** Basic arities
  -- $arities

  , Arity (..)
  , B, G
  , A0, A1, A2, A3
  , DownTo
  , Visits (..)

  -- * Working with free variables
  -- $frees

  , subst
  , substMap
  , subst1
  , freeVars

  -- * Freshen class

  , Freshen (..)

  -- * 'View' in detail

  , View (..)
  , fold
  , unfold

) where

import Zabt.Arity
import Zabt.Freshen
import Zabt.Internal.Term
import Zabt.View
import Zabt.Visits

{- $intro

  Abstract binding trees take the form @'Term' v f a@, or, more commonly, @'Flat' v f@. These 
  types are abstract—you will never construct or analyze them directly.
 
 -}

{- $patterns

  To construct or analyze a 'Term', the 'Var', 'Abs', and 'Pat' pattern synonyms are useful. 
  These synonyms let you essentially treat 'Term' as if it weren't abstract and both construct 
  new terms and @case@ analyze them.
 
 -}

{- $arities

  When defining a pattern functor for 'Term's you have to declare the /arities/ of each 
  recursive use.
 
 -}

{- $frees

  Abstract binding trees take the form @'Term' v f a@, or, more commonly, @'Flat' v f@. These 
  types are abstract---you will never construct or analyze them directly.
 
 -}
