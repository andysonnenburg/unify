{-# LANGUAGE
    FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Control.Monad.Ref.Class
       ( MonadRef (..)
       ) where

import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Error.Wrap
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Ref.Int as Int
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Lazy as Lazy

import Data.Monoid

import Data.IORef
import Data.STRef

class Monad m => MonadRef ref m | m -> ref where
  newRef :: a -> m (ref a)
  readRef :: ref a -> m a
  writeRef :: ref a -> a -> m ()
  modifyRef :: ref a -> (a -> a) -> m ()
  modifyRef ref f = writeRef ref . f =<< readRef ref

instance MonadRef IORef IO where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef
  modifyRef = modifyIORef

instance MonadRef (STRef s) (ST s) where
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef
  modifyRef = modifySTRef

instance Monad m => MonadRef (Int.Ref s) (Int.RefSupplyT s m) where
  newRef = Int.newRef
  readRef = Int.readRef
  writeRef = Int.writeRef
  modifyRef = Int.modifyRef

instance (Error e, MonadRef ref m) => MonadRef ref (ErrorT e m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
  modifyRef ref = lift . modifyRef ref

instance MonadRef ref m => MonadRef ref (ReaderT r m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
  modifyRef ref = lift . modifyRef ref

instance MonadRef ref m => MonadRef ref (Lazy.StateT s m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
  modifyRef ref = lift . modifyRef ref

instance MonadRef ref m => MonadRef ref (WrappedErrorT e m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
  modifyRef ref = lift . modifyRef ref

instance (Monoid w, MonadRef ref m) => MonadRef ref (Lazy.WriterT w m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
  modifyRef ref = lift . modifyRef ref
