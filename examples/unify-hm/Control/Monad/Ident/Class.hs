{-# LANGUAGE PolyKinds #-}
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

import Data.Index as Exports

class Monad m => MonadIdent m where
  newIdent :: Index a -> m (Ident s a)

instance Monad m => MonadIdent (IdentSupplyT s m) where
  newIdent = Ident.newIdent

instance MonadIdent m => MonadIdent (ReaderT r m) where
  newIdent = lift . newIdent

instance MonadIdent m => MonadIdent (Lazy.StateT s m) where
  newIdent = lift . newIdent

instance MonadIdent m => MonadIdent (RefSupplyT s m) where
  newIdent = lift . newIdent

instance MonadIdent m => MonadIdent (Hashable.RefSupplyT m) where
  newIdent = lift . newIdent

instance MonadIdent m => MonadIdent (WrappedErrorT e m) where
  newIdent = lift . newIdent

instance MonadIdent m => MonadIdent (WrappedMonadT m) where
  newIdent = lift . newIdent
