{-# LANGUAGE LambdaCase #-}
module Control.Monad.Trans.Error.Wrap
       ( WrappedError (..)
       , WrappedErrorT (..)
       , runWrappedErrorT
       , throwError
       , catchError
       , mapWrappedErrorT
       , mapError
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.Error hiding (catchError, throwError)
import qualified Control.Monad.Trans.Error as Error

data WrappedError e
  = NoMsg
  | StrMsg String
  | Error !e deriving Show

instance Functor WrappedError where
  fmap _ NoMsg = NoMsg
  fmap _ (StrMsg s) = StrMsg s
  fmap f (Error e) = Error (f e)

instance Error (WrappedError e) where
  noMsg = NoMsg
  strMsg = StrMsg

newtype WrappedErrorT e m a
  = WrapErrorT { unwrapErrorT :: ErrorT (WrappedError e) m a
               }

runWrappedErrorT :: WrappedErrorT e m a -> m (Either (WrappedError e) a)
runWrappedErrorT = runErrorT . unwrapErrorT

instance Functor m => Functor (WrappedErrorT e m) where
  fmap f = WrapErrorT . fmap f . unwrapErrorT

instance (Functor m, Monad m) => Applicative (WrappedErrorT e m) where
  pure = WrapErrorT . return
  f <*> v = WrapErrorT $ unwrapErrorT f <*> unwrapErrorT v

instance (Functor m, Monad m) => Alternative (WrappedErrorT e m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => Monad (WrappedErrorT e m) where
  return = WrapErrorT . return
  m >>= k = WrapErrorT $ unwrapErrorT m >>= unwrapErrorT . k
  m >> n = WrapErrorT $ unwrapErrorT m >> unwrapErrorT n
  fail = WrapErrorT . fail

instance Monad m => MonadPlus (WrappedErrorT e m) where
  mzero = WrapErrorT mzero
  m `mplus` n = WrapErrorT $ unwrapErrorT m `mplus` unwrapErrorT n

instance MonadFix m => MonadFix (WrappedErrorT e m) where
  mfix = WrapErrorT . mfix . (unwrapErrorT .)

instance MonadTrans (WrappedErrorT e) where
  lift = WrapErrorT . lift

instance MonadIO m => MonadIO (WrappedErrorT e m) where
  liftIO = WrapErrorT . liftIO

throwError :: Monad m => e -> WrappedErrorT e m a
throwError = WrapErrorT . Error.throwError . Error

catchError :: Monad m =>
              WrappedErrorT e m a ->
              (e -> WrappedErrorT e m a) ->
              WrappedErrorT e m a
m `catchError` h =
  WrapErrorT $ unwrapErrorT m `Error.catchError` \ case
    e@NoMsg -> Error.throwError e
    e@(StrMsg _) -> Error.throwError e
    Error e -> unwrapErrorT $ h e

mapWrappedErrorT :: (m (Either (WrappedError e) a) -> n (Either (WrappedError e') b)) ->
                    WrappedErrorT e m a -> WrappedErrorT e' n b
mapWrappedErrorT f = WrapErrorT . mapErrorT f . unwrapErrorT

mapError :: Monad m => (e -> e') -> WrappedErrorT e m a -> WrappedErrorT e' m a
mapError f m = flip mapWrappedErrorT m . bind $ \ case
  Left NoMsg -> return $ Left NoMsg
  Left (StrMsg s) -> return . Left $ StrMsg s
  Left (Error e) -> return . Left . Error $ f e
  Right a -> return $ Right a

bind :: Monad m => (a -> m b) -> m a -> m b
bind = flip (>>=)
