{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Name
       ( module Exports
       ) where

import Control.Monad as Exports
import Control.Monad.Fix as Exports
import Control.Monad.Trans as Exports
import Control.Monad.Reader.Class
import Control.Monad.Ref.Class

import Control.Monad.Name.Class as Exports
import Control.Monad.Trans.Name as Exports (Name,
                                            NameSupply,
                                            runNameSupply,
                                            NameSupplyT,
                                            runNameSupplyT,
                                            mapNameSupplyT)

instance MonadRef ref m => MonadRef ref (NameSupplyT s m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
  modifyRef ref = lift . modifyRef ref

instance MonadReader r m => MonadReader r (NameSupplyT s m) where
  ask = lift ask
  local f = mapNameSupplyT $ local f
  reader = lift . reader
