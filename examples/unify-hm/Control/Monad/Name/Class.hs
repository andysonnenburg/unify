{-# LANGUAGE
    FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Control.Monad.Name.Class
       ( module Exports
       , MonadName (..)
       ) where

import qualified Control.Monad.Ref.Hashable as Hashable
import Control.Monad.Trans
import Control.Monad.Trans.Error.Wrap
import Control.Monad.Trans.Name hiding (newName)
import qualified Control.Monad.Trans.Name as Name
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Ref.Int
import qualified Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.Wrap

import Data.Proxy as Exports

class Monad m => MonadName name m | m -> name where
  newName :: Proxy a -> m (name a)

instance Monad m => MonadName (Name s) (NameSupplyT s m) where
  newName = Name.newName

instance MonadName name m => MonadName name (ReaderT r m) where
  newName = lift . newName

instance MonadName name m => MonadName name (Lazy.StateT s m) where
  newName = lift . newName

instance MonadName name m => MonadName name (RefSupplyT s m) where
  newName = lift . newName

instance MonadName name m => MonadName name (Hashable.RefSupplyT m) where
  newName = lift . newName

instance MonadName name m => MonadName name (WrappedErrorT e m) where
  newName = lift . newName

instance MonadName name m => MonadName name (WrappedMonadT m) where
  newName = lift . newName
