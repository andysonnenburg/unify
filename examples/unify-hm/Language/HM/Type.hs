{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable #-}
module Language.HM.Type
       ( Type (..)
       , TypeScheme (..)
       ) where

import Control.Monad.Unify

import Data.Foldable
import Data.Traversable

data Type a f
  = Var a
  | Fn f f
  | Bool deriving (Show, Functor, Foldable, Traversable)

instance Eq a => Unifiable (Type a) where
  zipMatch = go
    where
      go (Var a) (Var b) | a == b = Just (Var a)
      go (Fn a b) (Fn a' b') = Just (Fn (a, a') (b, b'))
      go Bool Bool = Just Bool
      go _ _ = Nothing

data TypeScheme a f
  = Forall [a] (Type a f) deriving Show