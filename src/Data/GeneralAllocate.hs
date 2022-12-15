{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{- |
Description : General datatypes for resource allocation and release.
Copyright   : Copyright 2022 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com

This module provides a general interface for describing the allocation and
release of some resource in some monadic context.

For /using/ the resources safely, see "Control.Monad.With" and
"Control.Monad.Allocate".

This design and implementation is heavily based on t'Data.Acquire.Internal.Acquire'
from [resourcet](https://github.com/snoyberg/conduit/tree/master/resourcet),
including some code copied verbatim and then generalized appropriately. @resourcet@
is @Copyright (c)2011, Michael Snoyman@, and licensed under the BSD 3-clause license
available at [LICENSE.resourcet](https://github.com/shlevy/general-allocate/blob/master/LICENSE.resourcet).
-}
module Data.GeneralAllocate where

import Control.Monad
import {-# SOURCE #-} Control.Monad.With

{- | Describe the allocation and release of an @a@ in @m@.

In monads which allow this, the allocation action is run with exceptions masked.
The argument to the action allows the allocation to run some action with the masking
state restored to its prior state. In monads without masking, the argument is 'id'.

[@m@]: The monad to allocate in
[@e@]: A data type for a failure condition, typically t'Control.Exception.Safe.SomeException'
[@releaseReturn@]: State to be returned upon resource release. Mainly useful for proper threading
                   of monadic state in error conditions.
[@releaseArg@]: An argument to be passed to the release action upon successful completion of usage.
[@a@]: The type of the resource
-}
newtype GeneralAllocate m e releaseReturn releaseArg a
  = GeneralAllocate ((∀ x. m x → m x) → m (GeneralAllocated m e releaseReturn releaseArg a))

{- | A resource allocated and releasable in @m@.

[@m@]: The monad to allocate in
[@e@]: A data type for a failure condition, typically t'Control.Exception.Safe.SomeException'
[@releaseReturn@]: State to be returned upon resource release. Mainly useful for proper threading
                   of monadic state in error conditions.
[@releaseArg@]: An argument to be passed to the release action upon successful completion of usage.
[@a@]: The type of the resource
-}
data GeneralAllocated m e releaseReturn releaseArg a = GeneralAllocated
  { allocatedResource ∷ !a
  -- ^ The allocated resource
  , releaseAllocated ∷ !(GeneralReleaseType e releaseArg → m releaseReturn)
  -- ^ The action to release the allocated resource
  }

{- | Types of release requests that can occur.

[@e@]: A data type for a failure condition, typically t'Control.Exception.Safe.SomeException'
[@a@]: A data type for success conditions
-}
data GeneralReleaseType e a
  = -- | The resource was used successfully
    ReleaseSuccess !a
  | -- | Some kind of error occured while the resource was held.
    --
    -- The error need not have originated from using the resource itself.
    ReleaseFailure !e
  deriving stock (Functor)

instance Functor (GeneralAllocated m e releaseReturn releaseArg) where
  f `fmap` (GeneralAllocated x rel) = GeneralAllocated (f x) rel

instance (Functor m) ⇒ Functor (GeneralAllocate m e releaseReturn releaseArg) where
  -- Refactoring `\restore → fmap f <$> alloc restore` to `fmap (fmap f) . alloc)` causes type inference failure
  {- HLINT ignore "Use fmap" -}
  f `fmap` (GeneralAllocate alloc) = GeneralAllocate $ \restore → fmap f <$> alloc restore

instance (MonadWith m, Monoid releaseReturn, e ~ WithException m) ⇒ Applicative (GeneralAllocate m e releaseReturn releaseArg) where
  pure a = GeneralAllocate $ \_ → pure . GeneralAllocated a . const $ pure mempty
  (<*>) = ap

instance (MonadWith m, Monoid releaseReturn, e ~ WithException m) ⇒ Monad (GeneralAllocate m e releaseReturn releaseArg) where
  return = pure
  (GeneralAllocate allocX) >>= f = GeneralAllocate $ \restore → do
    GeneralAllocated x releaseX ← allocX restore
    let GeneralAllocate allocY = f x
    GeneralAllocated y releaseY ← allocY restore `onFailure` (releaseX . ReleaseFailure)
    pure $
      GeneralAllocated y $ \relTy →
        uncurry (<>) <$> generalFinally (releaseY relTy) (releaseX relTy)
