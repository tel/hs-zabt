
-- | Example variable name class you may want to use.
module Zabt.Name where

import Data.String

import Zabt.Freshen

data Name = Name Int String
  deriving (Eq, Ord)

instance Show Name where
  show (Name n s) 
    | n == 0 = '\'' : s
    | otherwise = '\'' : s ++ show n

instance IsString Name where
  fromString = Name 0

instance Freshen Name where
  freshen (Name n s) = Name (n + 1) s
