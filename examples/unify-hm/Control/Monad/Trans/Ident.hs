{-# LANGUAGE Rank2Types #-}
module Control.Monad.Trans.Ident
       ( module Exports
       , Ident
       , IdentSupply
       , runIdentSupply
       , IdentSupplyT
       , runIdentSupplyT
       , newIdent
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Functor.Identity
import Data.Hashable
import Data.Proxy as Exports

newtype Ident s a = Ident { unIdent :: S } deriving (Show, Eq)

instance Hashable (Ident s a) where
  hash = hash . unIdent
  hashWithSalt salt = hashWithSalt salt . unIdent

type IdentSupply s = IdentSupplyT s Identity

runIdentSupply :: (forall s . IdentSupply s a) -> a
runIdentSupply = runIdentity . runIdentSupplyT

newtype IdentSupplyT s m a
  = IdentSupplyT { unIdentSupplyT :: StateT S m a
                 }

instance Functor m => Functor (IdentSupplyT s m) where
  fmap f = IdentSupplyT . fmap f . unIdentSupplyT

instance (Functor m, Monad m) => Applicative (IdentSupplyT s m) where
  pure = IdentSupplyT . pure
  f <*> a = IdentSupplyT $ unIdentSupplyT f <*> unIdentSupplyT a
  a *> b = IdentSupplyT $ unIdentSupplyT a *> unIdentSupplyT b
  a <* b = IdentSupplyT $ unIdentSupplyT a <* unIdentSupplyT b

instance (Functor m, MonadPlus m) => Alternative (IdentSupplyT s m) where
  empty = IdentSupplyT empty
  a <|> b = IdentSupplyT $ unIdentSupplyT a <|> unIdentSupplyT b

instance Monad m => Monad (IdentSupplyT s m) where
  return = IdentSupplyT . return
  m >>= k = IdentSupplyT $ unIdentSupplyT m >>= unIdentSupplyT . k
  m >> n = IdentSupplyT $ unIdentSupplyT m >> unIdentSupplyT n
  fail = IdentSupplyT . fail

instance MonadFix m => MonadFix (IdentSupplyT s m) where
  mfix = IdentSupplyT . mfix . (unIdentSupplyT .)

instance MonadTrans (IdentSupplyT s) where
  lift = IdentSupplyT . lift

instance MonadIO m => MonadIO (IdentSupplyT s m) where
  liftIO = lift . liftIO

runIdentSupplyT :: Monad m => (forall s . IdentSupplyT s m a) -> m a
runIdentSupplyT m = evalStateT (unIdentSupplyT m) 0

newIdent :: Monad m => Proxy a -> IdentSupplyT s m (Ident s a)
newIdent _ = IdentSupplyT $ do
  s <- get
  put $! s + 1
  return $ Ident s

type S = Integer
