{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Ref
       ( module Exports
       ) where

import Control.Monad as Exports
import Control.Monad.Error.Class
import Control.Monad.Fix as Exports
import Control.Monad.Trans as Exports

import Control.Monad.Ref.Class as Exports
import Control.Monad.Trans.Ref.Int (liftCatch)
import Control.Monad.Trans.Ref.Int as Exports (Ref,
                                               RefSupply,
                                               runRefSupply,
                                               RefSupplyT,
                                               runRefSupplyT)

instance MonadError e m => MonadError e (RefSupplyT s m) where
  throwError = lift . throwError
  catchError = liftCatch catchError
