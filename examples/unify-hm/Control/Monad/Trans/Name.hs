{-# LANGUAGE PolyKinds, Rank2Types #-}
module Control.Monad.Trans.Name
       ( module Exports
       , Name
       , NameSupply
       , runNameSupply
       , NameSupplyT
       , runNameSupplyT
       , newName
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Functor.Identity
import Data.Hashable
import Data.Proxy as Exports

newtype Name s a = Name { getName :: S } deriving (Show, Eq)

instance Hashable (Name s a) where
  hash = hash . getName
  hashWithSalt salt = hashWithSalt salt . getName

type NameSupply s = NameSupplyT s Identity

runNameSupply :: (forall s . NameSupply s a) -> a
runNameSupply = runIdentity . runNameSupplyT

newtype NameSupplyT (s :: *) (m :: * -> *) (a :: *)
  = NameSupplyT { unNameSupplyT :: StateT S m a
                }

instance Functor m => Functor (NameSupplyT s m) where
  fmap f = NameSupplyT . fmap f . unNameSupplyT

instance (Functor m, Monad m) => Applicative (NameSupplyT s m) where
  pure = NameSupplyT . pure
  f <*> a = NameSupplyT $ unNameSupplyT f <*> unNameSupplyT a
  a *> b = NameSupplyT $ unNameSupplyT a *> unNameSupplyT b
  a <* b = NameSupplyT $ unNameSupplyT a <* unNameSupplyT b

instance (Functor m, MonadPlus m) => Alternative (NameSupplyT s m) where
  empty = NameSupplyT empty
  a <|> b = NameSupplyT $ unNameSupplyT a <|> unNameSupplyT b

instance Monad m => Monad (NameSupplyT s m) where
  return = NameSupplyT . return
  m >>= k = NameSupplyT $ unNameSupplyT m >>= unNameSupplyT . k
  m >> n = NameSupplyT $ unNameSupplyT m >> unNameSupplyT n
  fail = NameSupplyT . fail

instance MonadFix m => MonadFix (NameSupplyT s m) where
  mfix = NameSupplyT . mfix . (unNameSupplyT .)

instance MonadTrans (NameSupplyT s) where
  lift = NameSupplyT . lift

instance MonadIO m => MonadIO (NameSupplyT s m) where
  liftIO = lift . liftIO

runNameSupplyT :: Monad m => (forall s . NameSupplyT s m a) -> m a
runNameSupplyT m = evalStateT (unNameSupplyT m) 0

newName :: Monad m => Proxy a -> NameSupplyT s m (Name s a)
newName _ = NameSupplyT $ do
  s <- get
  put $! s + 1
  return $ Name s

type S = Integer
