
# Zabt

*Simple-minded abstract binding trees.*

A utility library for language authors.

## Example

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module STLC where

import Zabt
import Zabt.Name

data Alg x 
  = App x x
  | Lam x
  deriving (Eq, Ord, Show, Functor, Foldable)

type Exp = Term Name Alg

pattern (:$) :: Exp -> Exp -> Exp
pattern (:$) f x = Pat (App f x) 

pattern (:\) :: Name -> Exp -> Exp
pattern (:\) v e = Pat (Lam (Abs v e))

infixr 0 :\
infixr 1 :$

ex1, ex2, ex3 :: Exp

ex1 = Var "foo"
ex2 = "foo" :\ Var "foo"
ex3 = ex2 :$ ex1

whnf :: Exp -> Exp
whnf x = case x of
  Var _ -> x
  _ :\ _ -> x
  (v :\ e) :$ x -> subst1 (v, x) e
```

## Similar libraries

- [**Bound**](https://hackage.haskell.org/package/bound)
- [**Unbound**](https://hackage.haskell.org/package/unbound)
- [**abt**](https://hackage.haskell.org/package/abt)
