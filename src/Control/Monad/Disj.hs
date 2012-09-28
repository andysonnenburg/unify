{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Disj
       ( module Exports
       , DisjT
       , runDisjT
       ) where

import Control.Monad as Exports
import Control.Monad.Catch.Class
import Control.Monad.Error.Class
import Control.Monad.Fix as Exports
import Control.Monad.Instances ()
import Control.Monad.Trans as Exports
import Control.Monad.Trans.Disj

instance (MonadPlus m, MonadError e m) => MonadError e (DisjT m) where
  throwError =
    lift . throwError
  m `catchError` h =
    lift $ runDisjT m mplus `catchError` (flip runDisjT mplus . h)

instance (MonadPlus m, MonadThrow e m) => MonadThrow e (DisjT m)
instance ( MonadCatch e m n
         , MonadPlus m
         , MonadPlus n
         ) => MonadCatch e (DisjT m) (DisjT n) where
  m `catch` h =
    lift $ runDisjT m mplus `catch` (flip runDisjT mplus . h)
