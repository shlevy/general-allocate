# Revision history for general-allocate

## 0.2.3.1 -- 2024-01-12

* Relax `base` bound for GHC 9.6

## 0.2.3.0 -- 2024-01-12

* `MonadWith` instance for `CatchT`

## 0.2.2.0 -- 2024-01-11

* Allow newer versions of some dependencies
* Fixing deriving MonadWith instances

## 0.2.1.4 -- 2023-03-01

* Allow older `primitive`

## 0.2.1.3 -- 2023-03-01

* Fix build with GHC 8.10

## 0.2.1.2 -- 2023-02-27

* Update `primitive` dependency

## 0.2.1.1 -- 2023-01-17

* Warn about https://gitlab.haskell.org/ghc/ghc/-/issues/16478 in `MonadWithExceptable` haddock

## 0.2.1.0 -- 2023-01-17

* Make `MonadWithExceptable` a newclass for use with QuantifiedConstraints

## 0.2.0.1 -- 2023-01-10

* Fix build with mtl 2.3

## 0.2.0.0 -- 2022-12-20

* Break out `Exceptable` out from `MonadWithExceptable`

## 0.1.1.0 -- 2022-12-20

* Add `transformers` instances for `MonadWith`.
* Add `MonadWithExceptable`

## 0.1.0.0 -- 2022-12-14

* First version.
