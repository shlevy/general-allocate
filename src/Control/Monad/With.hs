{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad
import Control.Monad.Except
import qualified Control.Monad.RWS.Lazy as L
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.ST
import qualified Control.Monad.State.Lazy as L
import Control.Monad.State.Strict
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource.Internal
import qualified Control.Monad.Writer.Lazy as L
import Control.Monad.Writer.Strict
import Data.Exceptable
import Data.Functor
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
  rel (ReleaseFailure e) = void $ err e
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

{- | A 'MonadWith' whose exception type can be projected into the Haskell exception hierarchy

Until https://gitlab.haskell.org/ghc/ghc/-/issues/16478 is fixed, you probably want to
add @{\-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-\}@ to modules using this.
-}
class (MonadWith m, Exceptable (WithException m)) ⇒ MonadWithExceptable m

instance (MonadWith m, Exceptable (WithException m)) ⇒ MonadWithExceptable m

instance MonadWith IO where
  stateThreadingGeneralWith (GeneralAllocate allocA) go = mask $ \restore → do
    GeneralAllocated a releaseA ← allocA restore
    b ←
      restore (go a) `catch` \e → do
        _ ← releaseA $ ReleaseFailure e
        throwM e
    c ← releaseA $ ReleaseSuccess b
    pure (b, c)

{- | A helper for [DerivingVia](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html) a
'MonadWith' and 'MonadWithExceptable' instance for any 'Monad'.

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
  stateThreadingGeneralWith (GeneralAllocate allocA) go = WithNoContinuation $ do
    let WithNoContinuation allocA' = allocA id
    GeneralAllocated a releaseA ← allocA'
    let WithNoContinuation go' = go a
    b ← go'
    let WithNoContinuation releaseA' = releaseA $ ReleaseSuccess b
    c ← releaseA'
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
  stateThreadingGeneralWith (GeneralAllocate allocA) go = ReaderT $ \r → do
    let
      allocA' ∷ (∀ x. m x → m x) → m (GeneralAllocated m (WithException m) releaseReturn b a)
      allocA' restore = do
        let
          restore' ∷ ∀ x. ReaderT r m x → ReaderT r m x
          restore' mx = ReaderT $ restore . runReaderT mx
        GeneralAllocated a releaseA ← runReaderT (allocA restore') r
        let
          releaseA' relTy = runReaderT (releaseA relTy) r
        pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (flip runReaderT r . go)

instance (MonadWith m) ⇒ MonadWith (ResourceT m) where
  type WithException (ResourceT m) = WithException m
  stateThreadingGeneralWith
    ∷ ∀ a b releaseReturn
     . GeneralAllocate (ResourceT m) (WithException m) releaseReturn b a
    → (a → ResourceT m b)
    → ResourceT m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = ResourceT $ \st → do
    let
      allocA' ∷ (∀ x. m x → m x) → m (GeneralAllocated m (WithException m) releaseReturn b a)
      allocA' restore = do
        let
          restore' ∷ ∀ x. ResourceT m x → ResourceT m x
          restore' mx = ResourceT $ restore . unResourceT mx
        GeneralAllocated a releaseA ← unResourceT (allocA restore') st
        let
          releaseA' relTy = unResourceT (releaseA relTy) st
        pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (flip unResourceT st . go)

instance MonadWith (Either e) where
  type WithException (Either e) = EitherException e
  stateThreadingGeneralWith (GeneralAllocate allocA) go = do
    GeneralAllocated a releaseA ← allocA id
    b ← case go a of
      Left e → do
        _ ← releaseA . ReleaseFailure $ EitherException e
        Left e
      x → x
    c ← releaseA $ ReleaseSuccess b
    pure (b, c)

instance MonadWith m ⇒ MonadWith (MaybeT m) where
  type WithException (MaybeT m) = Maybe (WithException m)
  stateThreadingGeneralWith
    ∷ ∀ a b releaseReturn
     . GeneralAllocate (MaybeT m) (Maybe (WithException m)) releaseReturn b a
    → (a → MaybeT m b)
    → MaybeT m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = MaybeT $ do
    (mb, mc) ← stateThreadingGeneralWith (GeneralAllocate allocA') $ \case
      Just a → runMaybeT $ go a
      Nothing → pure Nothing
    pure $ (,) <$> mb <*> mc
   where
    allocA' ∷ (∀ x. m x → m x) → m (GeneralAllocated m (WithException m) (Maybe releaseReturn) (Maybe b) (Maybe a))
    allocA' restore =
      runMaybeT (allocA restore') <&> \case
        Just (GeneralAllocated a releaseA) → GeneralAllocated (Just a) $ \case
          ReleaseSuccess (Just b) → runMaybeT . releaseA $ ReleaseSuccess b
          ReleaseFailure e → runMaybeT . releaseA . ReleaseFailure $ Just e
          _ → runMaybeT . releaseA $ ReleaseFailure Nothing
        Nothing → GeneralAllocated Nothing (const $ pure Nothing)
     where
      restore' ∷ ∀ x. MaybeT m x → MaybeT m x
      restore' = MaybeT . restore . runMaybeT

instance MonadWith m ⇒ MonadWith (ExceptT e m) where
  type WithException (ExceptT e m) = Either e (WithException m)
  stateThreadingGeneralWith
    ∷ ∀ a b releaseReturn
     . GeneralAllocate (ExceptT e m) (Either e (WithException m)) releaseReturn b a
    → (a → ExceptT e m b)
    → ExceptT e m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = ExceptT $ do
    (eb, ec) ← stateThreadingGeneralWith (GeneralAllocate allocA') $ \case
      Left e → pure $ Left e
      Right a → runExceptT $ go a
    pure $ do
      c ← ec
      b ← eb
      pure (b, c)
   where
    allocA' ∷ (∀ x. m x → m x) → m (GeneralAllocated m (WithException m) (Either e releaseReturn) (Either e b) (Either e a))
    allocA' restore =
      runExceptT (allocA restore') <&> \case
        Right (GeneralAllocated a releaseA) → GeneralAllocated (Right a) $ \case
          ReleaseSuccess (Right b) → runExceptT . releaseA $ ReleaseSuccess b
          ReleaseSuccess (Left e) → runExceptT . releaseA . ReleaseFailure $ Left e
          ReleaseFailure e → runExceptT . releaseA . ReleaseFailure $ Right e
        Left e → GeneralAllocated (Left e) (const . pure $ Left e)
     where
      restore' ∷ ∀ x. ExceptT e m x → ExceptT e m x
      restore' = ExceptT . restore . runExceptT

instance MonadWith m ⇒ MonadWith (IdentityT m) where
  type WithException (IdentityT m) = WithException m
  stateThreadingGeneralWith
    ∷ ∀ a b releaseReturn
     . GeneralAllocate (IdentityT m) (WithException m) releaseReturn b a
    → (a → IdentityT m b)
    → IdentityT m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = IdentityT $ do
    stateThreadingGeneralWith (GeneralAllocate allocA') $ runIdentityT . go
   where
    allocA' ∷ (∀ x. m x → m x) → m (GeneralAllocated m (WithException m) releaseReturn b a)
    allocA' restore =
      runIdentityT (allocA restore') <&> \case
        GeneralAllocated a releaseA → GeneralAllocated a $ runIdentityT . releaseA
     where
      restore' ∷ ∀ x. IdentityT m x → IdentityT m x
      restore' = IdentityT . restore . runIdentityT

instance MonadWith m ⇒ MonadWith (L.StateT s m) where
  type WithException (L.StateT s m) = (WithException m, s)
  stateThreadingGeneralWith
    ∷ ∀ a b releaseReturn
     . GeneralAllocate (L.StateT s m) (WithException m, s) releaseReturn b a
    → (a → L.StateT s m b)
    → L.StateT s m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = L.StateT $ \s0 → do
    let allocA' ∷ (∀ x. m x → m x) → m (GeneralAllocated m (WithException m) (releaseReturn, s) (b, s) (a, s))
        allocA' restore =
          L.runStateT (allocA restore') s0 <&> \case
            (GeneralAllocated a releaseA, s1) → GeneralAllocated (a, s1) $ \case
              ReleaseSuccess (b, s2) → L.runStateT (releaseA $ ReleaseSuccess b) s2
              ReleaseFailure e → L.runStateT (releaseA $ ReleaseFailure (e, s1)) s1
         where
          restore' ∷ ∀ x. L.StateT s m x → L.StateT s m x
          restore' mx = L.StateT $ restore . L.runStateT mx
    ((b, _s2), (c, s3)) ← stateThreadingGeneralWith (GeneralAllocate allocA') $ \case
      (a, s1) → L.runStateT (go a) s1
    pure ((b, c), s3)

instance MonadWith m ⇒ MonadWith (StateT s m) where
  type WithException (StateT s m) = (WithException m, s)
  stateThreadingGeneralWith
    ∷ ∀ a b releaseReturn
     . GeneralAllocate (StateT s m) (WithException m, s) releaseReturn b a
    → (a → StateT s m b)
    → StateT s m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = StateT $ \s0 → do
    let allocA' ∷ (∀ x. m x → m x) → m (GeneralAllocated m (WithException m) (releaseReturn, s) (b, s) (a, s))
        allocA' restore =
          runStateT (allocA restore') s0 <&> \case
            (GeneralAllocated a releaseA, s1) → GeneralAllocated (a, s1) $ \case
              ReleaseSuccess (b, s2) → runStateT (releaseA $ ReleaseSuccess b) s2
              ReleaseFailure e → runStateT (releaseA $ ReleaseFailure (e, s1)) s1
         where
          restore' ∷ ∀ x. StateT s m x → StateT s m x
          restore' mx = StateT $ restore . runStateT mx
    ((b, _s2), (c, s3)) ← stateThreadingGeneralWith (GeneralAllocate allocA') $ \case
      (a, s1) → runStateT (go a) s1
    pure ((b, c), s3)

instance (MonadWith m, Monoid w) ⇒ MonadWith (L.WriterT w m) where
  type WithException (L.WriterT w m) = (WithException m, w)
  stateThreadingGeneralWith
    ∷ ∀ a b releaseReturn
     . GeneralAllocate (L.WriterT w m) (WithException m, w) releaseReturn b a
    → (a → L.WriterT w m b)
    → L.WriterT w m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = L.WriterT $ do
    let allocA' ∷ (∀ x. m x → m x) → m (GeneralAllocated m (WithException m) (releaseReturn, w) (b, w) (a, w))
        allocA' restore =
          L.runWriterT (allocA restore') <&> \case
            (GeneralAllocated a releaseA, w0) → GeneralAllocated (a, w0) $ \case
              ReleaseSuccess (b, w1) → do
                (c, w2) ← L.runWriterT . releaseA $ ReleaseSuccess b
                pure (c, w1 <> w2)
              ReleaseFailure e → do
                (c, w1) ← L.runWriterT . releaseA $ ReleaseFailure (e, w0)
                pure (c, w0 <> w1)
         where
          restore' ∷ ∀ x. L.WriterT w m x → L.WriterT w m x
          restore' = L.WriterT . restore . L.runWriterT
    ((b, _w1), (c, w2)) ← stateThreadingGeneralWith (GeneralAllocate allocA') $ \case
      (a, w0) → do
        (b, w1) ← L.runWriterT $ go a
        pure (b, w0 <> w1)
    pure ((b, c), w2)

instance (MonadWith m, Monoid w) ⇒ MonadWith (WriterT w m) where
  type WithException (WriterT w m) = (WithException m, w)
  stateThreadingGeneralWith
    ∷ ∀ a b releaseReturn
     . GeneralAllocate (WriterT w m) (WithException m, w) releaseReturn b a
    → (a → WriterT w m b)
    → WriterT w m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = WriterT $ do
    let allocA' ∷ (∀ x. m x → m x) → m (GeneralAllocated m (WithException m) (releaseReturn, w) (b, w) (a, w))
        allocA' restore =
          runWriterT (allocA restore') <&> \case
            (GeneralAllocated a releaseA, w0) → GeneralAllocated (a, w0) $ \case
              ReleaseSuccess (b, w1) → do
                (c, w2) ← runWriterT . releaseA $ ReleaseSuccess b
                pure (c, w1 <> w2)
              ReleaseFailure e → do
                (c, w1) ← runWriterT . releaseA $ ReleaseFailure (e, w0)
                pure (c, w0 <> w1)
         where
          restore' ∷ ∀ x. WriterT w m x → WriterT w m x
          restore' = WriterT . restore . runWriterT
    ((b, _w1), (c, w2)) ← stateThreadingGeneralWith (GeneralAllocate allocA') $ \case
      (a, w0) → do
        (b, w1) ← runWriterT $ go a
        pure (b, w0 <> w1)
    pure ((b, c), w2)

instance (MonadWith m, Monoid w) ⇒ MonadWith (L.RWST r w s m) where
  type WithException (L.RWST r w s m) = (WithException m, s, w)
  stateThreadingGeneralWith
    ∷ ∀ a b releaseReturn
     . GeneralAllocate (L.RWST r w s m) (WithException m, s, w) releaseReturn b a
    → (a → L.RWST r w s m b)
    → L.RWST r w s m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = L.RWST $ \r s0 → do
    let allocA' ∷ (∀ x. m x → m x) → m (GeneralAllocated m (WithException m) (releaseReturn, s, w) (b, s, w) (a, s, w))
        allocA' restore =
          L.runRWST (allocA restore') r s0 <&> \case
            (GeneralAllocated a releaseA, s1, w0) → GeneralAllocated (a, s1, w0) $ \case
              ReleaseSuccess (b, s2, w1) → do
                (c, s3, w2) ← L.runRWST (releaseA $ ReleaseSuccess b) r s2
                pure (c, s3, w1 <> w2)
              ReleaseFailure e → do
                (c, s2, w1) ← L.runRWST (releaseA $ ReleaseFailure (e, s1, w0)) r s1
                pure (c, s2, w0 <> w1)
         where
          restore' ∷ ∀ x. L.RWST r w s m x → L.RWST r w s m x
          restore' mx = L.RWST $ \r' → restore . L.runRWST mx r'
    ((b, _s2, _w1), (c, s3, w2)) ← stateThreadingGeneralWith (GeneralAllocate allocA') $ \case
      (a, s1, w0) → do
        (b, s2, w1) ← L.runRWST (go a) r s1
        pure (b, s2, w0 <> w1)
    pure ((b, c), s3, w2)

instance (MonadWith m, Monoid w) ⇒ MonadWith (RWST r w s m) where
  type WithException (RWST r w s m) = (WithException m, s, w)
  stateThreadingGeneralWith
    ∷ ∀ a b releaseReturn
     . GeneralAllocate (RWST r w s m) (WithException m, s, w) releaseReturn b a
    → (a → RWST r w s m b)
    → RWST r w s m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = RWST $ \r s0 → do
    let allocA' ∷ (∀ x. m x → m x) → m (GeneralAllocated m (WithException m) (releaseReturn, s, w) (b, s, w) (a, s, w))
        allocA' restore =
          runRWST (allocA restore') r s0 <&> \case
            (GeneralAllocated a releaseA, s1, w0) → GeneralAllocated (a, s1, w0) $ \case
              ReleaseSuccess (b, s2, w1) → do
                (c, s3, w2) ← runRWST (releaseA $ ReleaseSuccess b) r s2
                pure (c, s3, w1 <> w2)
              ReleaseFailure e → do
                (c, s2, w1) ← runRWST (releaseA $ ReleaseFailure (e, s1, w0)) r s1
                pure (c, s2, w0 <> w1)
         where
          restore' ∷ ∀ x. RWST r w s m x → RWST r w s m x
          restore' mx = RWST $ \r' → restore . runRWST mx r'
    ((b, _s2, _w1), (c, s3, w2)) ← stateThreadingGeneralWith (GeneralAllocate allocA') $ \case
      (a, s1, w0) → do
        (b, s2, w1) ← runRWST (go a) r s1
        pure (b, s2, w0 <> w1)
    pure ((b, c), s3, w2)
