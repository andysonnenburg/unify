{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Control.Monad.Error.Wrap
       ( module Exports
       , WrappedError (..)
       , WrappedErrorT (..)
       , runWrappedErrorT
       ) where

import Control.Monad.Error as Exports
import Control.Monad.Ref.Class

data WrappedError e
  = NoMsg
  | StrMsg String
  | Error !e deriving Show

instance Error (WrappedError e) where
  noMsg = NoMsg
  strMsg = StrMsg

newtype WrappedErrorT e m a
  = WrapErrorT { unwrapErrorT :: ErrorT (WrappedError e) m a
               }

runWrappedErrorT :: WrappedErrorT e m a -> m (Either (WrappedError e) a)
runWrappedErrorT = runErrorT . unwrapErrorT

instance Monad m => Monad (WrappedErrorT e m) where
  return = WrapErrorT . return
  m >>= k = WrapErrorT $ unwrapErrorT m >>= unwrapErrorT . k
  m >> n = WrapErrorT $ unwrapErrorT m >> unwrapErrorT n
  fail = WrapErrorT . fail

instance Monad m => MonadError e (WrappedErrorT e m) where
  throwError = WrapErrorT . throwError . Error
  m `catchError` h = WrapErrorT $ unwrapErrorT m `catchError` h'
    where
      h' e@NoMsg = throwError e
      h' e@(StrMsg _) = throwError e
      h' (Error e) = unwrapErrorT $ h e

instance MonadFix m => MonadFix (WrappedErrorT e m) where
  mfix = WrapErrorT . mfix . (unwrapErrorT .)

instance MonadTrans (WrappedErrorT e) where
  lift = WrapErrorT . lift

instance MonadIO m => MonadIO (WrappedErrorT e m) where
  liftIO = WrapErrorT . liftIO

instance MonadRef ref m => MonadRef ref (WrappedErrorT e m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
  modifyRef ref = lift . modifyRef ref
