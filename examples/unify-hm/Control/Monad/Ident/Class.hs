{-# LANGUAGE
    FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Control.Monad.Ident.Class
       ( module Exports
       , MonadIdent (..)
       ) where

import qualified Control.Monad.Ref.Hashable as Hashable
import Control.Monad.Trans
import Control.Monad.Trans.Error.Wrap
import Control.Monad.Trans.Ident hiding (newIdent)
import qualified Control.Monad.Trans.Ident as Ident
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Ref.Int
import qualified Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.Wrap

import Data.Proxy as Exports

class Monad m => MonadIdent i m | m -> i where
  newIdent :: Proxy a -> m (i a)

instance Monad m => MonadIdent (Ident s) (IdentSupplyT s m) where
  newIdent = Ident.newIdent

instance MonadIdent i m => MonadIdent i (ReaderT r m) where
  newIdent = lift . newIdent

instance MonadIdent i m => MonadIdent i (Lazy.StateT s m) where
  newIdent = lift . newIdent

instance MonadIdent i m => MonadIdent i (RefSupplyT s m) where
  newIdent = lift . newIdent

instance MonadIdent i m => MonadIdent i (Hashable.RefSupplyT m) where
  newIdent = lift . newIdent

instance MonadIdent i m => MonadIdent i (WrappedErrorT e m) where
  newIdent = lift . newIdent

instance MonadIdent i m => MonadIdent i (WrappedMonadT m) where
  newIdent = lift . newIdent
