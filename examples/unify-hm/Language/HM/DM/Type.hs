{-# LANGUAGE
    DeriveFoldable
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

type Set = HashSet

data Poly name mono = Forall (Set (name Type)) mono deriving Functor
deriving instance (Show (name Type), Show mono) => Show (Poly name mono)

type Rho = Mono

data Mono name mono
  = Int
  | Fn mono mono
  | Var (name Type) deriving (Functor, Foldable, Traversable)
deriving instance (Show (name Type), Show mono) => Show (Mono name mono)

instance Eq (name Type) => Unifiable (Mono name) where
  zipMatch = go
    where
      go Int Int = Just Int
      go (Fn a b) (Fn a' b') = Just (Fn (a, a') (b, b'))
      go (Var a) (Var a') | a == a' = Just (Var a)
      go _ _ = Nothing
