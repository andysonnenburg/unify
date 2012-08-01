{-# LANGUAGE
    MultiParamTypeClasses
  , TypeFamilies
  , UndecidableInstances #-}
module Control.Monad.Ref.Hashable
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

import Data.Hashable

data Ref ref a = Ref !(ref a) !Integer

instance Eq (Ref ref a) where
  Ref _ x == Ref _ y = x == y

instance Ord (Ref ref a) where
  compare (Ref _ x) (Ref _ y) = compare x y

instance Show (Ref ref a) where
  show (Ref _ x) = show x

instance Hashable (Ref ref a) where
  hash (Ref _ x) = hash x
  hashWithSalt salt (Ref _ x) = hashWithSalt salt x

newtype RefSupplyT m a =
  RefSupplyT { unRefSupplyT :: StateT S m a
             }

runRefSupplyT :: Monad m => RefSupplyT m a -> m a
runRefSupplyT = flip evalStateT 0 . unRefSupplyT

type S = Integer

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
