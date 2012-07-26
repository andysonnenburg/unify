{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Control.Monad.Trans.Unifier.Map
       ( module Exports
       , Key (..)
       ) where

import Data.Functor.Plus as Exports
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Map.Lazy as Map

import Control.Monad.Trans.Unifier.Base as Exports

class Plus (Map k) => Key k where
  type Map k :: * -> *
  insert :: k -> v -> Map k v -> Map k v
  lookup :: k -> Map k v -> Maybe v

instance Ord a => Key (WrappedOrd a) where
  type Map (WrappedOrd a) = Map.Map (WrappedOrd a)
  insert = Map.insert
  lookup = Map.lookup

instance Key Int where
  type Map Int = IntMap
  insert = IntMap.insert
  lookup = IntMap.lookup
