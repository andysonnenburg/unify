{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Ident
       ( module Exports
       ) where

import Control.Monad as Exports
import Control.Monad.Fix as Exports
import Control.Monad.Trans as Exports
import Control.Monad.Ref.Class

import Control.Monad.Ident.Class as Exports
import Control.Monad.Trans.Ident as Exports (Ident,
                                             IdentSupply,
                                             runIdentSupply,
                                             IdentSupplyT,
                                             runIdentSupplyT)

instance MonadRef ref m => MonadRef ref (IdentSupplyT s m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
  modifyRef ref = lift . modifyRef ref
