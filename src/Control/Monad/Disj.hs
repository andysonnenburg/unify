{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Disj
       ( module Exports
       ) where

import Control.Monad as Exports
import Control.Monad.Error.Class
import Control.Monad.Fix as Exports
import Control.Monad.Trans as Exports
import Control.Monad.Trans.Disj as Exports

instance (MonadPlus m, MonadError e m) => MonadError e (DisjT m) where
  throwError =
    lift . throwError
  m `catchError` h =
    lift $ runDisjT m mplus `catchError` (flip runDisjT mplus . h)
