
module Zabt.Freshen where

import Data.Set (Set)
import qualified Data.Set as Set

-- | A type which can be freshened has an operation which attempts to find a
-- unique version of its input. The principal thing that must hold is that
-- `freshen n /= n`. It's not necessary that `freshen n` be totally fresh with
-- respect to a context---that's too much to ask of a value---but it is
-- necessary that `freshen` *eventually* produces a fresh value.
--
-- Variable identifier types must be instances of Freshen.
class Eq v => Freshen v where
  freshen :: v -> v

instance Freshen Int where
  freshen n = n + 1

-- | Freshen a variable until it can pass a given predicate.
freshenUntil :: Freshen v => (v -> Bool) -> (v -> v)
freshenUntil used = go where
  go v = if used v then go (freshen v) else v

-- | Freshen a variable with respect to a set of variables. Or, freshen a
-- variable until it's unique with respect to a set.
freshWrt :: (Ord v, Freshen v) => Set v -> (v -> v)
freshWrt used = freshenUntil (`Set.member` used)

