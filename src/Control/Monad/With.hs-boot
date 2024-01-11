{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Monad.With where

import Control.Exception.Safe
import Data.Coerce
import {-# SOURCE #-} Data.GeneralAllocate

class (Monad m, ∀ x y. Coercible x y ⇒ Coercible (m x) (m y)) ⇒ MonadWith m where
  type WithException m
  type WithException m = SomeException
  stateThreadingGeneralWith
    ∷ GeneralAllocate m (WithException m) releaseReturn b a
    → (a → m b)
    → m (b, releaseReturn)

onFailure ∷ (MonadWith m) ⇒ m a → (WithException m → m b) → m a
generalFinally ∷ (MonadWith m) ⇒ m a → m b → m (a, b)
