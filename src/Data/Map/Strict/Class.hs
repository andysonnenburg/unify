{-# LANGUAGE TypeFamilies #-}
module Data.Map.Strict.Class
       ( MapKey (..)
       , WrappedOrd (..)
       , WrappedHashable (..)
       ) where

import qualified Data.Map.Strict as Map
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap

class MapKey k where
  data Map k :: * -> *
  empty :: Map k v
  lookup :: k -> Map k v -> Maybe v
  insert :: k -> v -> Map k v -> Map k v
  member :: k -> Map k v -> Bool

newtype WrappedOrd k
  = WrapOrd { unwrapOrd :: k
            } deriving (Eq, Ord)

instance Ord k => MapKey (WrappedOrd k) where
  newtype Map (WrappedOrd k) v
    = Map { getMap :: Map.Map (WrappedOrd k) v
          }
  empty = Map Map.empty
  lookup k = Map.lookup k . getMap
  insert k v = Map . Map.insert k v . getMap
  member k = Map.member k . getMap

newtype WrappedHashable k
  = WrapHashable { unwrapHashable :: k
                 } deriving Eq

instance Hashable k => Hashable (WrappedHashable k) where
  hash = hash . unwrapHashable
  hashWithSalt salt = hashWithSalt salt . unwrapHashable

instance (Eq k, Hashable k) => MapKey (WrappedHashable k) where
  newtype Map (WrappedHashable k) v
    = HashMap { getHashMap :: HashMap.HashMap (WrappedHashable k) v
              }
  empty = HashMap HashMap.empty
  lookup k = HashMap.lookup k . getHashMap
  insert k v = HashMap . HashMap.insert k v . getHashMap
  member k = HashMap.member k . getHashMap

instance MapKey Int where
  newtype Map Int v
    = IntMap { getIntMap :: IntMap.IntMap v
             }
  empty = IntMap IntMap.empty
  lookup k = IntMap.lookup k . getIntMap
  insert k v = IntMap . IntMap.insert k v . getIntMap
  member k = IntMap.member k . getIntMap