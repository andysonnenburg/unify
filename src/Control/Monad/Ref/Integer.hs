{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Ref.Integer
       ( module Exports
       , Ref
       , RefSupplyT
       , runRefSupplyT
       ) where

import Control.Monad as Exports
import Control.Monad.Catch.Class
import Control.Monad.Error.Class
import Control.Monad.Fix as Exports
import Control.Monad.Instances ()
import Control.Monad.Ref.Class
import Control.Monad.Trans as Exports

import Control.Monad.Trans.Ref.Integer (Ref,
                                        RefSupplyT,
                                        runRefSupplyT)
import qualified Control.Monad.Trans.Ref.Integer as Integer

instance MonadError e m => MonadError e (RefSupplyT s m) where
  throwError = lift . throwError
  catchError = Integer.liftCatch catchError

instance Monad m => MonadRef (Ref s) (RefSupplyT s m) where
  newRef = Integer.newRef
  readRef = Integer.readRef
  writeRef = Integer.writeRef
  modifyRef = Integer.modifyRef

instance MonadThrow e m => MonadThrow e (RefSupplyT s m)
instance MonadCatch e m n =>
         MonadCatch e (RefSupplyT s m) (RefSupplyT s n) where
  catch = Integer.liftCatch catch
