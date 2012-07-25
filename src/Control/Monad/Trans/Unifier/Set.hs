{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Control.Monad.Trans.Unifier.Set
       ( module Exports
       , Elem (..)
       ) where

import Data.Monoid as Exports
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

class Monoid (Set e) => Elem e where
  type Set e
  insert :: e -> Set e -> Set e
  member :: e -> Set e -> Bool

instance Elem Int where
  type Set Int = IntSet
  insert = IntSet.insert
  member = IntSet.member
