{-# LANGUAGE UnicodeSyntax #-}

{- |
Description : MonadAllocate-conferring transformer for no-continuation monads
Copyright   : Copyright 2022 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com

This module defines a 'MonadTrans'former, 'NoContinuationResourceT', which allows for
running t'Control.Monad.Allocate.MonadAllocate' code in pure 'PrimMonad's. This allows
for writing monad-generic resource-safe code and freely switching between
t'Control.Monad.Trans.Resource.ResourceT' on top of 'IO' and the pure 'NoContinuationResourceT'
on top of 'Control.Monad.ST.ST'.
-}
module Control.Monad.NoContinuation.Resource
  ( NoContinuationResourceT
  , runNoContinuationResourceT
  , StupidlyManyResources (..)
  )
where

import Control.Monad
import Control.Monad.NoContinuation.Resource.Internal
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.With
import Data.GeneralAllocate
import Data.IntMap.Strict as IntMap
import Data.Primitive.MutVar

-- | Run a 'NoContinuationResourceT' computation, freeing all resources before continuing.
runNoContinuationResourceT ∷ (PrimMonad m, MonadWith m) ⇒ NoContinuationResourceT m a → m a
runNoContinuationResourceT (NoContinuationResourceT r) = do
  let cleanup' [] _ = pure ()
      cleanup' ((_, rel) : tl) res = fst <$> generalFinally (rel res) (cleanup' tl res)
      cleanup st res = do
        actions ← releaseActions <$> readMutVar st
        cleanup' (toAscList actions) (void res)
      alloc = GeneralAllocate $ \restore → do
        st ←
          restore . newMutVar $
            NoContinuationReleaseMap
              { nextKey = maxBound
              , releaseActions = IntMap.empty
              }
        pure $ GeneralAllocated st (cleanup st)
  generalWith alloc (runReaderT r)
