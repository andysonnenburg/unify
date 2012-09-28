{-# LANGUAGE
    DataKinds
  , EmptyDataDecls
  , KindSignatures
  , Rank2Types
  , RecordWildCards #-}
module Control.Monad.Trans.Ref.Integer
       ( Region
       , Ref
       , RefSupply
       , runRefSupply
       , RefSupplyT
       , runRefSupplyT
       , newRef
       , readRef
       , writeRef
       , modifyRef
       , liftCatch
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import qualified Control.Monad.Trans.State.Strict as State

import Data.Functor.Identity
import Data.Hashable
import Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as Map

import GHC.Exts (Any)

import Unsafe.Coerce (unsafeCoerce)

type Map = HashMap

data Region

newtype Ref (s :: Region) a = Ref { unRef :: Integer } deriving (Eq, Show)

instance Hashable (Ref s a) where
  hash = hash . unRef
  hashWithSalt salt = hashWithSalt salt . unRef

type RefSupply s = RefSupplyT s Identity

runRefSupply :: (forall s . RefSupply s a) -> a
runRefSupply = runIdentity . runRefSupplyT

newtype RefSupplyT (s :: Region) m a
  = RefSupplyT { unRefSupplyT :: StateT S m a
               }

instance Functor m => Functor (RefSupplyT s m) where
  fmap f = RefSupplyT . fmap f . unRefSupplyT
  a <$ m = RefSupplyT $ a <$ unRefSupplyT m

instance (Functor m, Monad m) => Applicative (RefSupplyT s m) where
  pure = RefSupplyT . pure
  f <*> a = RefSupplyT $ unRefSupplyT f <*> unRefSupplyT a
  a *> b = RefSupplyT $ unRefSupplyT a *> unRefSupplyT b
  a <* b = RefSupplyT $ unRefSupplyT a <* unRefSupplyT b

instance (Functor m, MonadPlus m) => Alternative (RefSupplyT s m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => Monad (RefSupplyT s m) where
  return = RefSupplyT . return
  m >>= k = RefSupplyT $ unRefSupplyT m >>= unRefSupplyT . k
  m >> n = RefSupplyT $ unRefSupplyT m >> unRefSupplyT n
  fail = RefSupplyT . fail

instance MonadPlus m => MonadPlus (RefSupplyT s m) where
  mzero = RefSupplyT mzero
  m `mplus` n = RefSupplyT $ unRefSupplyT m `mplus` unRefSupplyT n

instance MonadFix m => MonadFix (RefSupplyT s m) where
  mfix = RefSupplyT . mfix . (unRefSupplyT .)

instance MonadTrans (RefSupplyT s) where
  lift = RefSupplyT . lift

instance MonadIO m => MonadIO (RefSupplyT s m) where
  liftIO = lift . liftIO

data S
  = S { refCount :: !Integer
      , refMap :: Map Integer Any
      }

initS :: S
initS = S { refCount = 0, refMap = Map.empty }

get :: Monad m => RefSupplyT s m S
get = RefSupplyT State.get

gets :: Monad m => (S -> a) -> RefSupplyT s m a
gets = RefSupplyT . State.gets

modify :: Monad m => (S -> S) -> RefSupplyT s m ()
modify = RefSupplyT . State.modify

put :: Monad m => S -> RefSupplyT s m ()
put = RefSupplyT . State.put

runRefSupplyT :: Monad m => (forall s . RefSupplyT s m a) -> m a
runRefSupplyT m = evalStateT (unRefSupplyT m) initS

newRef :: Monad m => a -> RefSupplyT s m (Ref s a)
newRef a = do
  S {..} <- get
  put S { refCount = refCount + 1
        , refMap = Map.insert refCount (unsafeCoerce a) refMap
        }
  return $ Ref refCount

readRef :: Monad m => Ref s a -> RefSupplyT s m a
readRef ref = gets $ unsafeCoerce . (!unRef ref) . refMap

writeRef :: Monad m => Ref s a -> a -> RefSupplyT s m ()
writeRef ref a = modify f
  where
    f s@S {..} =
      s { refMap = Map.insert (unRef ref) (unsafeCoerce a) refMap }

modifyRef :: Monad m => Ref s a -> (a -> a) -> RefSupplyT s m ()
modifyRef ref f =
  modify $ \ s@S {..} -> s { refMap = Map.adjust f' (unRef ref) refMap }
  where
    f' = unsafeCoerce . f . unsafeCoerce

liftCatch :: (forall a' . m a' -> (e -> n a') -> n a') ->
             RefSupplyT s m a ->
             (e -> RefSupplyT s n a) ->
             RefSupplyT s n a
liftCatch catch m h = RefSupplyT $ State.StateT $ \ s ->
  State.runStateT (unRefSupplyT m) s `catch` \ e ->
  State.runStateT (unRefSupplyT $ h e) s
