{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

{- |
Description : Lexically-scoped exception-safe resource usage
Copyright   : Copyright 2022 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com

This module defines interfaces for safe resource usage on top of 'GeneralAllocate',
where resource cleanup happens at the end of a lexical scope.

For contexts where nested scope-based allocation and release is insufficient, see
"Control.Monad.Allocate".
-}
module Control.Monad.With where

import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.Trans.Resource.Internal
import Data.Functor.Identity
import Data.GeneralAllocate
import Data.Void

{- | A monad allowing for exception-safe resource usage within a lexical scope.

The guarantees of 'MonadWith' are weaker than t'Control.Monad.Catch.MonadMask': in some
monads, it's possible for resources not to get cleaned up if the /entire/ monadic
computation is going to be aborted (e.g. an async exception sent to a thread executing a monad
with no exception catching). Of course, t'Control.Monad.Catch.MonadMask' itself can't guarantee
cleanup in the presence of @SIGKILL@... In any case, this allows for 'MonadWith' to be implemented
lawfully in more monads (see 'WithNoContinuation'). In particular, the 'MonadWith' instances for
'IO', 'ST', and 'Identity' allow for writing monad-generic exception-safe code which can be properly
instantiated in 'IO' or mocked out in 'ST'/'Identity' without changing the code.
-}
class Monad m ⇒ MonadWith m where
  -- | Data characterizing exceptional exit from the scope.
  type WithException m

  type WithException m = SomeException

  -- | Allocate, use, and release a resource in some scope, threading through some state.
  --
  -- If resource acquisition succeeds, the resource is guaranteed to be released
  -- /if the monadic computation itself is going to continue/. This is weaker than
  -- the guarantees of v'Control.Monad.Catch.generalBracket', which can't be
  -- implemented in monads without exception catching.
  --
  -- See 'generalWith' for the common use case where state threading isn't needed.
  stateThreadingGeneralWith
    ∷ GeneralAllocate m (WithException m) releaseReturn b a
    -- ^ Allocate the resource
    → (a → m b)
    -- ^ Use the resource
    → m (b, releaseReturn)

{- | Describe the allocation and release of a resource.

A specialization of 'GeneralAllocate' for the most
common case with 'MonadWith', see 'generalWith'.
-}
type With m = GeneralAllocate m (WithException m) ()

{- | Allocate, use, and release a resource in some scope.

If resource acquisition succeeds, the resource is guaranteed to be released
/if the monadic computation itself is going to continue/. This is weaker than
the guarantees of v'Control.Monad.Catch.generalBracket', which can't be
implemented in monads without exception catching.
-}
generalWith
  ∷ (MonadWith m)
  ⇒ With m b a
  -- ^ Allocate the resource
  → (a → m b)
  -- ^ Use the resource
  → m b
generalWith alloc = (fst <$>) . stateThreadingGeneralWith alloc

{- | Run some action if the first action fails.

Exception propagation will continue after the failure action runs.

If failure occurs, the failure action is guaranteed to run
/if the monadic compuation itself is going to continue/. This is
weaker than the guranatess of v'Control.Monad.Catch.onError', which can't
be implemented in monads without exception catching.
-}
onFailure
  ∷ (MonadWith m)
  ⇒ m a
  -- ^ Main action
  → (WithException m → m b)
  -- ^ Failure action
  → m a
onFailure go err = generalWith alloc (const go)
 where
  alloc = GeneralAllocate $ \_ → pure $ GeneralAllocated () rel
  rel (ReleaseFailure e) = err e >> pure ()
  rel _ = pure ()

{- | Run some action after another one completes in an exception-safe manner.

The final action is guaranteed to run
/if the monadic compuation itself is going to continue/. This is
weaker than the guranatess of v'Control.Monad.Catch.finally', which can't
be implemented in monads without exception catching.
-}
generalFinally
  ∷ (MonadWith m)
  ⇒ m a
  -- ^ Main action
  → m b
  -- ^ Final action
  → m (a, b)
generalFinally go fin = stateThreadingGeneralWith alloc $ const go
 where
  alloc = GeneralAllocate $ \_ → pure $ GeneralAllocated () rel
  rel _ = fin

instance MonadWith IO where
  stateThreadingGeneralWith (GeneralAllocate allocArg) go = mask $ \restore → do
    GeneralAllocated res releaseRes ← allocArg restore
    b ←
      restore (go res) `catch` \e → do
        _ ← releaseRes $ ReleaseFailure e
        throwM e
    c ← releaseRes $ ReleaseSuccess b
    pure (b, c)

{- | A helper for [DerivingVia](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html) a
'MonadWith' instance for any 'Monad'.

Note that the derived instance is only valid if the monad satisfies the "no continuation" condition, i.e.
that if execution of a computation exits a given lexical scope we are guaranteed that either all of the
actions within that scope have executed or the entire monadic computation has been terminated.

The most common factors violating "no continuation" are call/cc and exception catching. A monad which allows
exception /throwing/ but not catching is not thereby disqualified, as any thrown exception will of necessity
propagate until it terminates the entire monadic computation.
-}
newtype WithNoContinuation m a = WithNoContinuation (m a) deriving newtype (Functor, Applicative, Monad)

instance (Monad m) ⇒ MonadWith (WithNoContinuation m) where
  type WithException (WithNoContinuation m) = Void
  stateThreadingGeneralWith (GeneralAllocate allocArg) go = WithNoContinuation $ do
    let WithNoContinuation allocArg' = allocArg id
    GeneralAllocated res releaseRes ← allocArg'
    let WithNoContinuation go' = go res
    b ← go'
    let WithNoContinuation releaseRes' = releaseRes $ ReleaseSuccess b
    c ← releaseRes'
    pure (b, c)

deriving via WithNoContinuation (ST s) instance MonadWith (ST s)

deriving via WithNoContinuation Identity instance MonadWith Identity

instance (MonadWith m) ⇒ MonadWith (ReaderT r m) where
  type WithException (ReaderT r m) = WithException m
  stateThreadingGeneralWith
    ∷ ∀ a b releaseReturn
     . GeneralAllocate (ReaderT r m) (WithException m) releaseReturn b a
    → (a → ReaderT r m b)
    → ReaderT r m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocFun) go = ReaderT $ \r → do
    let
      allocFun' ∷ (∀ x. m x → m x) → m (GeneralAllocated m (WithException m) releaseReturn b a)
      allocFun' restore = do
        let
          restore' ∷ ∀ x. ReaderT r m x → ReaderT r m x
          restore' mx = ReaderT $ restore . runReaderT mx
        GeneralAllocated x release ← runReaderT (allocFun restore') r
        let
          release' relTy = runReaderT (release relTy) r
        pure $ GeneralAllocated x release'
    stateThreadingGeneralWith (GeneralAllocate allocFun') (flip runReaderT r . go)

instance (MonadWith m) ⇒ MonadWith (ResourceT m) where
  type WithException (ResourceT m) = WithException m
  stateThreadingGeneralWith
    ∷ ∀ a b releaseReturn
     . GeneralAllocate (ResourceT m) (WithException m) releaseReturn b a
    → (a → ResourceT m b)
    → ResourceT m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocFun) go = ResourceT $ \st → do
    let
      allocFun' ∷ (∀ x. m x → m x) → m (GeneralAllocated m (WithException m) releaseReturn b a)
      allocFun' restore = do
        let
          restore' ∷ ∀ x. ResourceT m x → ResourceT m x
          restore' mx = ResourceT $ restore . unResourceT mx
        GeneralAllocated x release ← unResourceT (allocFun restore') st
        let
          release' relTy = unResourceT (release relTy) st
        pure $ GeneralAllocated x release'
    stateThreadingGeneralWith (GeneralAllocate allocFun') (flip unResourceT st . go)
