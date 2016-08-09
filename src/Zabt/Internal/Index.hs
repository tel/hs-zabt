
-- | Newtype for de Bruijn indices.
module Zabt.Internal.Index where

import Zabt.Freshen

-- | A de Bruijn index.
newtype Index 
  = Index { value :: Int }
    deriving (Eq, Ord)

zero :: Index
zero = Index 0

next :: Index -> Index
next (Index i) = Index (i + 1)

instance Show Index where
  showsPrec p (Index i) = showsPrec p i

instance Freshen Index where
  freshen = next
