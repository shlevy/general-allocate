{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

{- |
Copyright   : Copyright 2022 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com

This design and implementation is heavily based on t'Control.Monad.Trans.Resource.Internal.ResourceT'
from [resourcet](https://github.com/snoyberg/conduit/tree/master/resourcet),
including some code copied verbatim and then generalized appropriately. @resourcet@
is @Copyright (c)2011, Michael Snoyman@, and licensed under the BSD 3-clause license
available at [LICENSE.resourcet](https://github.com/shlevy/general-allocate/blob/master/LICENSE.resourcet).
-}
module Control.Monad.NoContinuation.Resource.Internal where

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Allocate
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.With
import Control.Monad.Writer.Lazy
import Data.GeneralAllocate
import Data.IntMap.Strict as IntMap
import Data.Primitive.MutVar

{- | A 'MonadTrans'former turning any 'PrimMonad' that is a 'MonadWith' into a 'MonadAllocate'

Note that the 'MonadAllocate' instance is only valid if the underlying monad satisfies the "no continuation"
condition, i.e. that if execution of a computation exits a given lexical scope we are guaranteed that either
all of the actions within that scope have executed or the entire monadic computation has been terminated.

The most common factors violating "no continuation" are call/cc and exception catching. A monad which allows
exception /throwing/ but not catching is not thereby disqualified, as any thrown exception will of necessity
propagate until it terminates the entire monadic computation.

In conjunction with the 'PrimMonad' requirement, this essentially means the base of @m@ must be an 'Control.Monad.ST.ST' and
there must be no 'Control.Monad.Cont.ContT' in the stack.
-}
newtype NoContinuationResourceT m a = NoContinuationResourceT {unNoContinuationResourceT ∷ ReaderT (MutVar (PrimState m) (NoContinuationReleaseMap m)) m a}
  deriving newtype (Applicative, Functor, Monad, MonadPlus, PrimMonad, MonadFail, Alternative, MonadState s, MonadWriter w, MonadFix)

instance (MonadReader r m) ⇒ MonadReader r (NoContinuationResourceT m) where
  ask = lift ask
  local f (NoContinuationResourceT go) = NoContinuationResourceT . ReaderT $ local f . runReaderT go

instance MonadTrans NoContinuationResourceT where
  lift = NoContinuationResourceT . lift

-- | Caller tried to allocate more than @maxBound :: Int@ resources in a single 'NoContinuationResourceT' scope.
data StupidlyManyResources = StupidlyManyResources deriving stock (Show)

instance Exception StupidlyManyResources

-- | Internal state for 'NoContinuationResourceT'
data NoContinuationReleaseMap m = NoContinuationReleaseMap
  { nextKey ∷ !Key
  -- ^ The next key to allocate a release action for.
  --
  -- Invariant: key must be monotonically /decreasing/.
  , releaseActions ∷ !(IntMap (GeneralReleaseType (WithException m) () → m ()))
  -- ^ Map of release actions.
  }

-- | Handle to run a release action early in 'NoContinuationResourceT'
data NoContinuationReleaseKey m = NoContinuationReleaseKey
  { index ∷ !Key
  -- ^ The index of this action in the map
  , mapVar ∷ !(MutVar (PrimState m) (NoContinuationReleaseMap m))
  -- ^ A reference to the map, shared with the allocation context.
  }

instance (PrimMonad m, MonadWith m) ⇒ MonadAllocate (NoContinuationResourceT m) where
  type AllocationContext (NoContinuationResourceT m) = m
  type AllocationException (NoContinuationResourceT m) = WithException m
  type GeneralReleaseKey (NoContinuationResourceT m) = NoContinuationReleaseKey m
  generalAllocate (GeneralAllocate alloc) = do
    GeneralAllocated x rel ← lift $ alloc id
    mapVar ← NoContinuationResourceT ask
    index ← atomicModifyMutVar' mapVar $ \(NoContinuationReleaseMap{..}) →
      ( NoContinuationReleaseMap
          { nextKey = if nextKey == minBound then impureThrow StupidlyManyResources else pred nextKey
          , releaseActions = insert nextKey rel releaseActions
          }
      , nextKey
      )
    pure (NoContinuationReleaseKey{..}, x)
  generalRelease (NoContinuationReleaseKey{..}) = do
    m_rel ← atomicModifyMutVar' mapVar $ \(NoContinuationReleaseMap{..}) →
      ( NoContinuationReleaseMap
          { releaseActions = delete index releaseActions
          , ..
          }
      , IntMap.lookup index releaseActions
      )
    case m_rel of
      Nothing → pure ()
      Just rel → rel $ ReleaseSuccess ()
