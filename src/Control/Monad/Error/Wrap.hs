{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Error.Wrap
       ( module Exports
       ) where

import Control.Monad as Exports
import Control.Monad.Error.Class as Exports
import Control.Monad.Fix as Exports
import Control.Monad.Trans as Exports

import qualified Control.Monad.Trans.Error.Wrap as Wrap
import Control.Monad.Trans.Error.Wrap as Exports (WrappedError (..),
                                                  WrappedErrorT,
                                                  runWrappedErrorT)

instance Monad m => MonadError e (WrappedErrorT e m) where
  throwError = Wrap.throwError
  catchError = Wrap.catchError
