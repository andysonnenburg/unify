{-# LANGUAGE
    Rank2Types
  , RecordWildCards
  , TypeFamilies #-}
module Control.Monad.Trans.Ref.Int
       ( Ref
       , RefSupply
       , runRefSupply
       , RefSupplyT
       , runRefSupplyT
       , newRef
       , readRef
       , writeRef
       , modifyRef
       ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict hiding (get, gets, modify, put)
import qualified Control.Monad.Trans.State.Strict as State

import Data.Functor.Identity
import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap
import Data.Map.Strict.Class (Map, MapKey)
import qualified Data.Map.Strict.Class as Map
import Data.Set.Class (Set, SetElem)
import qualified Data.Set.Class as Set

import GHC.Exts (Any)

import Unsafe.Coerce (unsafeCoerce)

newtype Ref s a = Ref { unRef :: Int } deriving (Eq, Show)

instance MapKey (Ref s a) where
  newtype Map (Ref s a) v = RefMap { unRefMap :: Map Int v }
  empty = RefMap Map.empty
  lookup k = Map.lookup (unRef k) . unRefMap
  insert k v = RefMap . Map.insert (unRef k) v . unRefMap
  member k = Map.member (unRef k) . unRefMap

instance SetElem (Ref s a) where
  newtype Set (Ref s a) = RefSet { unRefSet :: Set Int }
  empty = RefSet Set.empty
  singleton = RefSet . Set.singleton . unRef
  insert e = RefSet . Set.insert (unRef e) . unRefSet
  member e = Set.member (unRef e) . unRefSet
  union x y = RefSet $ Set.union (unRefSet x) (unRefSet y)
  toList = map Ref . Set.toList . unRefSet

type RefSupply s = RefSupplyT s Identity

runRefSupply :: (forall s . RefSupply s a) -> a
runRefSupply = runIdentity . runRefSupplyT

newtype RefSupplyT s m a
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

instance Monad m => Monad (RefSupplyT s m) where
  return = RefSupplyT . return
  m >>= k = RefSupplyT $ unRefSupplyT m >>= unRefSupplyT . k
  m >> n = RefSupplyT $ unRefSupplyT m >> unRefSupplyT n
  fail = RefSupplyT . fail

instance MonadTrans (RefSupplyT s) where
  lift = RefSupplyT . lift

instance MonadIO m => MonadIO (RefSupplyT s m) where
  liftIO = lift . liftIO

data S
  = S { refCount :: !Int
      , refMap :: IntMap Any
      }

initS :: S
initS = S { refCount = 0, refMap = IntMap.empty }

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
        , refMap = IntMap.insert refCount (unsafeCoerce a) refMap
        }
  return $ Ref refCount

readRef :: Monad m => Ref s a -> RefSupplyT s m a
readRef ref = gets $ unsafeCoerce . (!unRef ref) . refMap

writeRef :: Monad m => Ref s a -> a -> RefSupplyT s m ()
writeRef ref a = modify f
  where
    f s@S {..} = s { refMap = IntMap.insert (unRef ref) (unsafeCoerce a) refMap }

modifyRef :: Monad m => Ref s a -> (a -> a) -> RefSupplyT s m ()
modifyRef ref f =
  modify $ \ s@S {..} -> s { refMap = IntMap.adjust f' (unRef ref) refMap }
  where
    f' = unsafeCoerce . f . unsafeCoerce
