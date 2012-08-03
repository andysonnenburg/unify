module Control.Monad.Trans.Disj
       ( DisjT
       , runDisjT
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

data DisjT m a
  = Lift !(m a)
  | Plus (DisjT m a) (DisjT m a) deriving Show

instance Functor m => Functor (DisjT m) where
  fmap f = go
    where
      go (Lift m) = Lift $ fmap f m
      go (Plus m n) = fmap f m `Plus` fmap f n

instance (Alternative m) => Applicative (DisjT m) where
  pure = Lift . pure
  f <*> a = Lift $ runDisjT f (<|>) <*> runDisjT a (<|>)

instance Alternative m => Alternative (DisjT m) where
  empty = Lift empty
  (<|>) = Plus

instance MonadPlus m => Monad (DisjT m) where
  return = lift . return
  (Lift m) >>= k = Lift $ m >>= flip runDisjT mplus . k
  (Plus m n) >>= k = Plus (m >>= k) (n >>= k)
  (Lift m) >> n = Lift $ m >> runDisjT n mplus
  (Plus m n) >> o = Plus (m >> o) (n >> o)
  fail = lift . fail

instance MonadPlus m => MonadPlus (DisjT m) where
  mzero = lift mzero
  mplus = Plus

instance (MonadPlus m, MonadFix m) => MonadFix (DisjT m) where
  mfix = lift . mfix . (flip runDisjT mplus .)

instance MonadTrans DisjT where
  lift = Lift

instance (MonadPlus m, MonadIO m) => MonadIO (DisjT m) where
  liftIO = lift . liftIO

runDisjT :: DisjT m a -> (m a -> m a -> m a) -> m a
runDisjT t plus = go t
  where
    go (Lift m) = m
    go (Plus m n) = go m `plus` go n
