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
import Control.Monad.Trans.Unifier.Map (Alt (..), Map, Plus (..))
import qualified Control.Monad.Trans.Unifier.Map as Map
import Control.Monad.Trans.Unifier.Set (Monoid (..), Set)
import qualified Control.Monad.Trans.Unifier.Set as Set

import Data.Functor.Identity
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap

import GHC.Exts (Any)

import Unsafe.Coerce (unsafeCoerce)

newtype Ref s a = Ref { unRef :: Int } deriving (Eq, Show)

newtype RefMap s v = RefMap { unRefMap :: Map Int v }

instance Functor (RefMap s) where
  fmap f = RefMap . fmap f . unRefMap

instance Alt (RefMap s) where
  a <!> b = RefMap $ unRefMap a <!> unRefMap b

instance Plus (RefMap s) where
  zero = RefMap zero

instance Map.Key (Ref s a) where
  type Map (Ref s a) = RefMap s
  insert k v = RefMap . Map.insert (unRef k) v . unRefMap
  lookup k = Map.lookup (unRef k) . unRefMap

newtype RefSet s = RefSet { unRefSet :: Set Int }

instance Monoid (RefSet s) where
  mempty = RefSet mempty
  a `mappend` b = RefSet $ unRefSet a `mappend` unRefSet b

instance Set.Elem (Ref s a) where
  type Set (Ref s a) = RefSet s
  insert e = RefSet . Set.insert (unRef e) . unRefSet
  member e = Set.member (unRef e) . unRefSet

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
