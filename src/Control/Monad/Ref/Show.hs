{-# LANGUAGE
    MultiParamTypeClasses
  , TypeFamilies
  , UndecidableInstances #-}
module Control.Monad.Ref.Show
       ( module Exports
       , Ref
       , RefSupplyT
       , runRefSupplyT
       ) where

import Control.Monad as Exports
import Control.Monad.Fix as Exports
import Control.Monad.Ref.Class
import Control.Monad.Trans as Exports
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Unifier.Map (Alt (..), Map, Plus (..))
import qualified Control.Monad.Trans.Unifier.Map as Map
import Control.Monad.Trans.Unifier.Set (Monoid (..), Set)
import qualified Control.Monad.Trans.Unifier.Set as Set

import qualified Data.IntMap as Map (elems)
import Data.Maybe

data Ref ref a = Ref !(ref a) !Int

instance Eq (Ref ref a) where
  Ref _ x == Ref _ y = x == y

instance Ord (Ref ref a) where
  compare (Ref _ x) (Ref _ y) = compare x y

instance Show (Ref ref a) where
  show (Ref _ x) = show x

newtype RefMap (ref :: * -> *) a v = RefMap { unRefMap :: Map Int v }

instance Functor (RefMap ref a) where
  fmap f = RefMap . fmap f . unRefMap

instance Alt (RefMap ref a) where
  a <!> b = RefMap $ unRefMap a <!> unRefMap b

instance Plus (RefMap ref a) where
  zero = RefMap zero

instance Map.Key (Ref ref a) where
  type Map (Ref ref a) = RefMap ref a
  insert (Ref _ k) v = RefMap . Map.insert k v . unRefMap
  lookup (Ref _ k) = Map.lookup k . unRefMap

newtype RefSet s a = RefSet { unRefSet :: Map Int (Ref s a) }

instance Monoid (RefSet s a) where
  mempty = RefSet zero
  a `mappend` b = RefSet $ unRefSet a <!> unRefSet b

instance Set.Elem (Ref s a) where
  type Set (Ref s a) = RefSet s a
  insert v@(Ref _ k) = RefSet . Map.insert k v . unRefSet
  member (Ref _ k) = isJust . Map.lookup k . unRefSet
  toList = Map.elems . unRefSet

newtype RefSupplyT m a =
  RefSupplyT { unRefSupplyT :: StateT S m a
             }

runRefSupplyT :: Monad m => RefSupplyT m a -> m a
runRefSupplyT = flip evalStateT 0 . unRefSupplyT

type S = Int

instance Monad m => Monad (RefSupplyT m) where
  return = RefSupplyT . return
  m >>= k = RefSupplyT $ unRefSupplyT m >>= unRefSupplyT . k
  m >> n = RefSupplyT $ unRefSupplyT m >> unRefSupplyT n
  fail = RefSupplyT . fail

instance MonadTrans RefSupplyT where
  lift = RefSupplyT . lift

instance MonadIO m => MonadIO (RefSupplyT m) where
  liftIO = RefSupplyT . liftIO

instance MonadRef ref m => MonadRef (Ref ref) (RefSupplyT m) where
  newRef a = do
    ref <- lift $ newRef a
    show' <- RefSupplyT get
    RefSupplyT $ put $ show' + 1
    return $ Ref ref show'
  readRef (Ref ref _) =
    lift $ readRef ref
  writeRef (Ref ref _) a =
    lift $ writeRef ref a
  modifyRef (Ref ref _) f =
    lift $ modifyRef ref f
