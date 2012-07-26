{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Control.Monad.Trans.Unifier.Set
       ( module Exports
       , Elem (..)
       ) where

import Data.Monoid as Exports
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import Control.Monad.Trans.Unifier.Base as Exports

class Monoid (Set e) => Elem e where
  type Set e
  insert :: e -> Set e -> Set e
  member :: e -> Set e -> Bool
  toList :: Set e -> [e]

instance Ord a => Elem (WrappedOrd a) where
  type Set (WrappedOrd a) = Set.Set (WrappedOrd a)
  insert = Set.insert
  member = Set.member
  toList = Set.toList

instance Elem Int where
  type Set Int = IntSet
  insert = IntSet.insert
  member = IntSet.member
  toList = IntSet.toList
