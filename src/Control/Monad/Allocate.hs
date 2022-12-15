{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

{- |
Description : Exception-safe arbitrary-lifetime resource usage with deterministic cleanup
Copyright   : Copyright 2022 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com

This module defines interfaces for safe resource usage on top of 'GeneralAllocate', where
resources have an arbitrary lifetime and can be released manually or at the end of the
computation.

For contexts where nested scope-based allocation and release is sufficient, see
"Control.Monad.With".

This design is heavily based on 'MonadResource' from [resourcet](https://github.com/snoyberg/conduit/tree/master/resourcet).
@resourcet@ is @Copyright (c)2011, Michael Snoyman@, and licensed under the BSD 3-clause license
available at [LICENSE.resourcet](https://github.com/shlevy/general-allocate/blob/master/LICENSE.resourcet).
-}
module Control.Monad.Allocate where

import Control.Exception.Safe
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.RWS.Lazy
import qualified Control.Monad.RWS.Strict as Strict
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource hiding (throwM)
import Control.Monad.Trans.Resource.Internal
import Control.Monad.Writer.Lazy
import qualified Control.Monad.Writer.Strict as Strict
import Data.Acquire
import Data.Acquire.Internal
import Data.GeneralAllocate
import Data.Kind

{- | A monad allowing for exception-safe resource usage with arbitrary lifetimes.

The guarantees of 'MonadAllocate' are weaker than 'MonadResource':
in some monads, it's possible for resources not to get cleaned up if the
/entire/ monadic computation is going to be aborted (e.g. an async exception
sent to a thread executing a monad with no exception catching). Of course,
'MonadResource' itself can't guarantee cleanup in the presence of @SIGKILL@...
In any case, this allows for 'MonadAllocate' to be implemented lawfully in more
monads (see t'Control.Monad.NoContinuation.Resource.NoContinuationResourceT'). This
allows for writing monad-generic exception-safe code which can be properly
instantiated in 'IO' or mocked out in 'Control.Monad.ST.ST' without changing the code.
-}
class (Monad m, Monad (AllocationContext m)) ⇒ MonadAllocate m where
  -- | The monad in which resources are allocated and released.
  type AllocationContext m ∷ Type → Type

  -- | A handle to release some resource early manually
  type GeneralReleaseKey m

  -- | The exception type of the monadic context
  type AllocationException m

  type AllocationException m = SomeException

  -- | Allocate some resource, which will be cleaned up on call to
  -- 'generalRelease' or the end of the current resource scope,
  -- whichever comes first.
  generalAllocate
    ∷ GeneralAllocate (AllocationContext m) (AllocationException m) () () a
    → m (GeneralReleaseKey m, a)

  -- | Register an action which will be guaranteed to run on call to
  -- 'generalRelease' or the end of the current resource scope,
  -- whichever comes first.
  generalRegister
    ∷ (GeneralReleaseType (AllocationException m) () → AllocationContext m ())
    → m (GeneralReleaseKey m)
  generalRegister rel = fst <$> (generalAllocate . GeneralAllocate $ alloc)
   where
    alloc ∷ (∀ x. AllocationContext m x → AllocationContext m x) → AllocationContext m (GeneralAllocated (AllocationContext m) (AllocationException m) () () ())
    alloc _ = pure $ GeneralAllocated () rel

  -- | Run a release action from a prior call to 'generalAllocate'/'generalRegister'.
  generalRelease
    ∷ GeneralReleaseKey m
    → AllocationContext m ()

{- | A helper for [DerivingVia](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html) a
'MonadAllocate' instance for any 'MonadResource'.
-}
newtype AllocateViaResource m a = AllocateViaResource (m a) deriving newtype (Functor, Applicative, Monad)

instance (MonadResource m) ⇒ MonadAllocate (AllocateViaResource m) where
  type AllocationContext (AllocateViaResource m) = IO
  type GeneralReleaseKey (AllocateViaResource m) = ReleaseKey

  generalAllocate
    ∷ ∀ a
     . GeneralAllocate (AllocationContext (AllocateViaResource m)) SomeException () () a
    → AllocateViaResource m (ReleaseKey, a)
  generalAllocate (GeneralAllocate alloc) = AllocateViaResource $ do
    let alloc' ∷ ((∀ b. IO b → IO b) → IO (Allocated a))
        alloc' restore = do
          GeneralAllocated x rel ← alloc restore
          let rel' (ReleaseExceptionWith e) = rel $ ReleaseFailure e
              rel' _ = rel $ ReleaseSuccess ()
          pure $ Allocated x rel'
    (k, x) ← allocateAcquire $ Acquire alloc'
    pure (k, x)

  generalRegister rel = AllocateViaResource . liftResourceT . withInternalState $ \st → do
    let rel' (ReleaseExceptionWith e) = rel $ ReleaseFailure e
        rel' _ = rel $ ReleaseSuccess ()
    registerType st rel'
  generalRelease = release

deriving via AllocateViaResource (ResourceT m) instance (MonadIO m) ⇒ MonadAllocate (ResourceT m)

{- | A helper for [DerivingVia](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html) a
'MonadAllocate' instance for any 'MonadTrans'formed 'MonadAllocate'.
-}
type AllocateViaLift ∷ ((Type → Type) → (Type → Type)) → (Type → Type) → Type → Type
newtype AllocateViaLift t m a = AllocateViaLift (t m a) deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance (MonadAllocate m, MonadTrans t, Monad (t m)) ⇒ MonadAllocate (AllocateViaLift t m) where
  type AllocationContext (AllocateViaLift t m) = AllocationContext m
  type GeneralReleaseKey (AllocateViaLift t m) = GeneralReleaseKey m
  type AllocationException (AllocateViaLift t m) = AllocationException m
  generalAllocate = lift . generalAllocate
  generalRegister = lift . generalRegister
  generalRelease = generalRelease @m

deriving via AllocateViaLift MaybeT m instance (MonadAllocate m) ⇒ MonadAllocate (MaybeT m)

deriving via AllocateViaLift (ExceptT e) m instance (MonadAllocate m) ⇒ MonadAllocate (ExceptT e m)

deriving via AllocateViaLift (ReaderT r) m instance (MonadAllocate m) ⇒ MonadAllocate (ReaderT r m)

deriving via AllocateViaLift (StateT s) m instance (MonadAllocate m) ⇒ MonadAllocate (StateT s m)

deriving via AllocateViaLift (Strict.StateT s) m instance (MonadAllocate m) ⇒ MonadAllocate (Strict.StateT s m)

deriving via AllocateViaLift (WriterT w) m instance (Monoid w, MonadAllocate m) ⇒ MonadAllocate (WriterT w m)

deriving via AllocateViaLift (Strict.WriterT w) m instance (Monoid w, MonadAllocate m) ⇒ MonadAllocate (Strict.WriterT w m)

deriving via AllocateViaLift (ContT r) m instance (MonadAllocate m) ⇒ MonadAllocate (ContT r m)

deriving via AllocateViaLift (RWST r w s) m instance (Monoid w, MonadAllocate m) ⇒ MonadAllocate (RWST r w s m)

deriving via AllocateViaLift (Strict.RWST r w s) m instance (Monoid w, MonadAllocate m) ⇒ MonadAllocate (Strict.RWST r w s m)
