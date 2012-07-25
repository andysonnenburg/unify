{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Control.Monad.Trans.Unifier.Map
       ( module Exports
       , Key (..)
       ) where

import Data.Functor.Plus as Exports
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

class Plus (Map k) => Key k where
  type Map k :: * -> *
  insert :: k -> v -> Map k v -> Map k v
  lookup :: k -> Map k v -> Maybe v

instance Key Int where
  type Map Int = IntMap
  insert = IntMap.insert
  lookup = IntMap.lookup
