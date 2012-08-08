{-# LANGUAGE
    DataKinds
  , DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , FlexibleContexts
  , StandaloneDeriving
  , UndecidableInstances #-}
module Language.HM.DM.Type
       ( Poly (..)
       , Rho
       , Mono (..)
       ) where

import Control.Monad.Unify

import Data.Foldable
import Data.HashSet (HashSet)
import Data.Traversable

import Language.HM.Var

data Poly a f = Forall (HashSet (a Type)) f deriving Functor
deriving instance (Show (a Type), Show f) => Show (Poly a f)

type Rho = Mono

data Mono a f
  = Int
  | Fn f f
  | Var (a Type) deriving (Functor, Foldable, Traversable)
deriving instance (Show (a Type), Show f) => Show (Mono a f)

instance Eq (a Type) => Unifiable (Mono a) where
  zipMatch = go
    where
      go Int Int = Just Int
      go (Fn a b) (Fn a' b') = Just (Fn (a, a') (b, b'))
      go (Var a) (Var a') | a == a' = Just (Var a)
      go _ _ = Nothing
