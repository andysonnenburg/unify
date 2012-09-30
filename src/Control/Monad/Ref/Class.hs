{-# LANGUAGE
    DefaultSignatures
  , FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Control.Monad.Ref.Class
       ( MonadRef (..)
       , modifyRefDefault
       ) where

import qualified Control.Monad.ST.Lazy as Lazy (ST)
import qualified Control.Monad.ST.Strict as Strict (ST)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Error (Error, ErrorT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)

import Data.IORef (IORef,
                   newIORef,
                   readIORef,
                   writeIORef,
                   modifyIORef)
import Data.Monoid (Monoid)
import qualified Data.STRef.Lazy as Lazy (STRef,
                                          newSTRef,
                                          readSTRef,
                                          writeSTRef,
                                          modifySTRef)
import qualified Data.STRef.Strict as Strict (STRef,
                                              newSTRef,
                                              readSTRef,
                                              writeSTRef,
                                              modifySTRef)

class Monad m => MonadRef ref m | m -> ref where
  newRef :: a -> m (ref a)
  default newRef :: (MonadRef ref m, MonadTrans t) => a -> t m (ref a)
  newRef = lift . newRef
  readRef :: ref a -> m a
  default readRef :: (MonadRef ref m, MonadTrans t) => ref a -> t m a
  readRef = lift . readRef
  writeRef :: ref a -> a -> m ()
  default writeRef :: (MonadRef ref m, MonadTrans t) => ref a -> a -> t m ()
  writeRef ref = lift . writeRef ref
  modifyRef :: ref a -> (a -> a) -> m ()
  default modifyRef :: (MonadRef ref m, MonadTrans t) => ref a -> (a -> a) -> t m ()
  modifyRef ref = lift . modifyRef ref

modifyRefDefault :: MonadRef ref m => ref a -> (a -> a) -> m ()
modifyRefDefault ref f = readRef ref >>= writeRef ref . f

instance MonadRef IORef IO where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef
  modifyRef = modifyIORef

instance MonadRef (Lazy.STRef s) (Lazy.ST s) where
  newRef = Lazy.newSTRef
  readRef = Lazy.readSTRef
  writeRef = Lazy.writeSTRef
  modifyRef = Lazy.modifySTRef

instance MonadRef (Strict.STRef s) (Strict.ST s) where
  newRef = Strict.newSTRef
  readRef = Strict.readSTRef
  writeRef = Strict.writeSTRef
  modifyRef = Strict.modifySTRef

instance MonadRef ref m => MonadRef ref (ContT r m)
instance (Error e, MonadRef ref m) => MonadRef ref (ErrorT e m)
instance MonadRef ref m => MonadRef ref (IdentityT m)
instance MonadRef ref m => MonadRef ref (ListT m)
instance MonadRef ref m => MonadRef ref (MaybeT m)
instance MonadRef ref m => MonadRef ref (ReaderT r m)
instance (Monoid w, MonadRef ref m) => MonadRef ref (Lazy.RWST r w s m)
instance (Monoid w, MonadRef ref m) => MonadRef ref (Strict.RWST r w s m)
instance MonadRef ref m => MonadRef ref (Lazy.StateT s m)
instance MonadRef ref m => MonadRef ref (Strict.StateT s m)
instance (Monoid w, MonadRef ref m) => MonadRef ref (Lazy.WriterT w m)
instance (Monoid w, MonadRef ref m) => MonadRef ref (Strict.WriterT w m)
