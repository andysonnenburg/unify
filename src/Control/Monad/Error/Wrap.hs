{-# LANGUAGE
    FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Error.Wrap
       ( module Exports
       , WrappedError (..)
       , WrappedErrorT (..)
       , runWrappedErrorT
       ) where

import Control.Monad as Exports
import Control.Monad.Catch.Class as Exports
import Control.Monad.Error.Class as Exports
import Control.Monad.Fix as Exports
import Control.Monad.Instances ()
import Control.Monad.Reader.Class
import Control.Monad.Ref.Class
import Control.Monad.Trans as Exports

import qualified Control.Monad.Trans.Error.Wrap as Wrap
import Control.Monad.Trans.Error.Wrap (WrappedError (..),
                                       WrappedErrorT (..),
                                       runWrappedErrorT)

instance Monad m => MonadError e (WrappedErrorT e m) where
  throwError = Wrap.throwError
  catchError = Wrap.catchError

instance MonadReader r m => MonadReader r (WrappedErrorT e m) where
  ask = lift ask
  local f = WrapErrorT . local f . unwrapErrorT
  reader = lift . reader

instance MonadRef ref m => MonadRef ref (WrappedErrorT e m)

instance Monad m => MonadThrow e (WrappedErrorT e m) where
  throw = WrapErrorT . throw . Error
instance Monad m => MonadCatch e (WrappedErrorT e m) (WrappedErrorT e' m) where
  m `catch` h = WrapErrorT $ unwrapErrorT m `catch` \ case
    NoMsg -> throw NoMsg
    StrMsg s -> throw $ StrMsg s
    Error e -> unwrapErrorT $ h e
