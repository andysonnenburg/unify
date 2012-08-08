module Control.Monad.Trans.Wrap
       ( WrappedMonadT (..)
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

newtype WrappedMonadT m a
  = WrapMonadT { unwrapMonadT :: m a
               }

instance Monad m => Functor (WrappedMonadT m) where
  fmap = liftM

instance Monad m => Applicative (WrappedMonadT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (WrappedMonadT m) where
  return = WrapMonadT . return
  m >>= k = WrapMonadT $ unwrapMonadT m >>= unwrapMonadT . k
  m >> n = WrapMonadT $ unwrapMonadT m >> unwrapMonadT n
  fail = WrapMonadT . fail

instance MonadTrans WrappedMonadT where
  lift = WrapMonadT
