{-# LANGUAGE TypeFamilies #-}
module Data.Set.Class
       ( module Exports
       , SetElem (..)
       ) where

import qualified Data.IntSet as IntSet
import Data.Map.Strict.Class as Exports (WrappedOrd (..), WrappedHashable (..))

class SetElem e where
  data Set e
  empty :: Set e
  singleton :: e -> Set e
  insert :: e -> Set e -> Set e
  union :: Set e -> Set e -> Set e
  toList :: Set e -> [e]

instance SetElem Int where
  newtype Set Int = IntSet { getIntSet :: IntSet.IntSet }
  empty = IntSet IntSet.empty
  singleton = IntSet . IntSet.singleton
  insert e = IntSet . IntSet.insert e . getIntSet
  union x y = IntSet $ IntSet.union (getIntSet x) (getIntSet y)
  toList = IntSet.toList . getIntSet
