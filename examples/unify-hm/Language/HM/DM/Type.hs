{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable #-}
module Language.HM.DM.Type
       ( Poly (..)
       , Rho
       , Mono (..)
       ) where

import Control.Monad.Unify

import Data.Foldable
import Data.HashSet (HashSet)
import Data.Traversable

data Poly a f = Forall (HashSet a) f deriving (Show, Functor)

type Rho = Mono

data Mono a f
  = Int
  | Fn f f
  | Var a deriving (Show, Functor, Foldable, Traversable)

instance Eq a => Unifiable (Mono a) where
  zipMatch = go
    where
      go Int Int = Just Int
      go (Fn a b) (Fn a' b') = Just (Fn (a, a') (b, b'))
      go (Var a) (Var a') | a == a' = Just (Var a)
      go _ _ = Nothing
