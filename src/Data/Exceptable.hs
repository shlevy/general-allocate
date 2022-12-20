{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UnicodeSyntax #-}

{- |
Description : Projections into the Haskell exception hierarchy
Copyright   : Copyright 2022 Shea Levy.
License     : Apache-2.0
Maintainer  : shea@shealevy.com

Provides 'Exceptable' for error types which can be projected into
the Haskell exception hierarchy.
-}
module Data.Exceptable where

import Control.Exception
import Data.Typeable
import Data.Void

-- | Types which can be projected into the Haskell exception hierarchy
class Exceptable e where
  toSomeException ∷ e → SomeException

instance Exceptable SomeException where
  toSomeException = id

instance Exceptable Void where
  toSomeException = absurd

-- | An 'Exception' representing a failure in the 'Either' monad.
newtype EitherException e = EitherException e deriving stock (Show)

instance (Show e, Typeable e) ⇒ Exception (EitherException e)

instance (Show e, Typeable e) ⇒ Exceptable (EitherException e) where
  toSomeException = toException

-- | An 'Exception' representing the 'Nothing' case in a 'Maybe' monad.
data NothingException = NothingException deriving (Show)

instance Exception NothingException

instance Exceptable e ⇒ Exceptable (Maybe e) where
  toSomeException (Just e) = toSomeException e
  toSomeException Nothing = toException NothingException

instance (Show e, Typeable e, Exceptable e') ⇒ Exceptable (Either e e') where
  toSomeException (Left e) = toException $ EitherException e
  toSomeException (Right e) = toSomeException e

instance Exceptable e ⇒ Exceptable (e, s) where
  toSomeException (e, _) = toSomeException e

instance Exceptable e ⇒ Exceptable (e, s, w) where
  toSomeException (e, _, _) = toSomeException e
