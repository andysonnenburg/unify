{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Wrap
       ( module Exports
       , WrappedMonadT (..)
       ) where

import Control.Monad as Exports
import Control.Monad.Error.Class
import Control.Monad.Fix as Exports
import Control.Monad.Ref.Class
import Control.Monad.Trans as Exports
import Control.Monad.Trans.Wrap as Exports

instance MonadError e m => MonadError e (WrappedMonadT m) where
  throwError =
    lift . throwError
  m `catchError` h =
    lift $ unwrapMonadT m `catchError` (unwrapMonadT . h)

instance MonadRef ref m => MonadRef ref (WrappedMonadT m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
  modifyRef ref = lift . modifyRef ref
