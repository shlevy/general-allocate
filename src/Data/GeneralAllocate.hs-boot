{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.GeneralAllocate where

import Data.Kind (Type)

type GeneralAllocate ∷ (Type → Type) → Type → Type → Type → Type → Type

type role GeneralAllocate nominal nominal nominal nominal nominal

data GeneralAllocate m e releaseReturn releaseArg a
