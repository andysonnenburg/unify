{-# LANGUAGE
    FlexibleContexts
  , Rank2Types
  , StandaloneDeriving
  , UndecidableInstances
  , ViewPatterns #-}
module Control.Monad.Trans.Unifier
       ( module Exports
       , Term
       , Var
       , Unifiable (..)
       , UnificationException (..)
       , freshTerm
       , unify
       , freeVars
       , freeze
       , unfreeze
       ) where

import Control.Monad hiding (mapM)
import Control.Monad.Error hiding (mapM)
import Control.Monad.Free as Exports
import Control.Monad.Ref.Class
import Control.Monad.State hiding (mapM)

import Data.Fix
import Data.Foldable
import Data.Map.Strict.Class (Map, MapKey)
import qualified Data.Map.Strict.Class as Map
import Data.Set.Class (Set, SetElem)
import qualified Data.Set.Class as Set
import Data.Traversable

import Prelude hiding (mapM)  

newtype Var f ref
  = Var { getRef :: ref (Maybe (Term f ref))
        }

type Term f ref = Free f (Var f ref)

data UnificationException f ref
  = OccursIn (ref (Maybe (Term f ref))) (f (Term f ref))
  | TermMismatch (f (Term f ref)) (f (Term f ref))
  | UnboundVar (ref (Maybe (Term f ref)))

deriving instance ( Show (f (Term f ref))
                  , Show (ref (Maybe (Term f ref)))
                  , Show (Term f ref)
                  ) => Show (UnificationException f ref)

instance Error (UnificationException f ref)

class Traversable f => Unifiable f where
  zipMatch :: f a -> f b -> Maybe (f (a, b))

deriving instance Show (ref (Maybe (Term f ref))) => Show (Var f ref)

freshTerm :: MonadRef ref m => m (Term f ref)
freshTerm = liftM (Pure . Var) $ newRef Nothing

unify :: ( Unifiable f
         , Eq (ref (Maybe (Term f ref)))
         , MapKey (ref (Maybe (Term f ref)))
         , MonadError (UnificationException f ref) m
         , MonadRef ref m
         ) => Term f ref -> Term f ref -> m (Term f ref)
unify = unify'
  where
    unify' t1 t2 =
      evalStateT (loop t1 t2) Map.empty
    loop t1 t2 =
      bindM2 (semiprune t1) (semiprune t2) loop'
    loop'
      (Semipruned _ (UnboundVarS r1))
      (Semipruned t2 (UnboundVarS r2))
      | r1 == r2 =
        return t2
      | otherwise = do
        writeRef r1 $ Just t2
        return t2
    loop'
      (Semipruned _ (UnboundVarS r1))
      (Semipruned t2 (BoundVarS _ _)) = do
        writeRef r1 $ Just t2
        return t2
    loop'
      (Semipruned t1 (BoundVarS _ _))
      (Semipruned _ (UnboundVarS r2)) = do
        writeRef r2 $ Just t1
        return t1
    loop'
      (Semipruned _ (BoundVarS r1 f1))
      (Semipruned t2 (BoundVarS r2 f2))
      | r1 == r2 =
        return t2
      | otherwise = do
        t <- localState $ do
          r1 `seenAs` f1
          r2 `seenAs` f2
          match f1 f2
        writeRef r2 $ Just t
        writeRef r1 $ Just t2
        return t2
    loop'
      (Semipruned t1 (UnboundVarS r1))
      (Semipruned t2 (TermS _)) = do
        writeRef r1 $ Just t2
        return t1
    loop'
      (Semipruned t1 (TermS _))
      (Semipruned t2 (UnboundVarS r2)) = do
        writeRef r2 $ Just t1
        return t2
    loop'
      (Semipruned t1 (BoundVarS r1 f1))
      (Semipruned _ (TermS f2)) = do
        f <- localState $ do
          r1 `seenAs` f1
          match f1 f2
        writeRef r1 $ Just f
        return t1
    loop'
      (Semipruned _ (TermS f1))
      (Semipruned t2 (BoundVarS r2 f2)) = do
        f <- localState $ do
          r2 `seenAs` f2
          match f1 f2
        writeRef r2 $ Just f
        return t2
    loop'
      (Semipruned _ (TermS f1))
      (Semipruned _ (TermS f2)) =
        match f1 f2
    match x y =
      maybe
      (throwError $ TermMismatch x y)
      (liftM Free . mapM (uncurry loop)) $
      zipMatch x y

data TermS f ref
  = Semipruned !(Term f ref) !(Semipruned f ref)

data Semipruned f ref
  = TermS !(f (Term f ref))
  | UnboundVarS !(ref (Maybe (Term f ref)))
  | BoundVarS !(ref (Maybe (Term f ref))) !(f (Term f ref))

semiprune :: MonadRef ref m => Term f ref -> m (TermS f ref)
semiprune = semiprune'
  where
    semiprune' t0@(Pure (getRef -> r0)) =
      loop t0 r0
    semiprune' t0@(Free f0) =
      return $ Semipruned t0 (TermS f0)
    loop t0 r0 =
      readRef r0 >>=
      maybe
      (return $ Semipruned t0 (UnboundVarS r0))
      (\ t -> case t of
          Pure (getRef -> r) -> do
            sp@(Semipruned t' _) <- loop t r
            writeRef r0 $ Just t'
            return sp
          Free f ->
            return $ Semipruned t0 (BoundVarS r0 f))

freeVars :: ( Foldable f
            , SetElem (ref (Maybe (Term f ref)))
            , MonadRef ref m
            ) => Term f ref -> m (Set (ref (Maybe (Term f ref))))
freeVars = foldlUnboundVarsM (\ a -> return . flip Set.insert a) Set.empty

foldlUnboundVarsM :: ( Foldable f
                     , MonadRef ref m
                     ) =>
                     (a -> ref (Maybe (Term f ref)) -> m a) -> a -> Term f ref -> m a
foldlUnboundVarsM k = foldlM go
  where
    go a (getRef -> r) =
      readRef r >>=
      maybe (k a r) (foldlM go a)

freeze :: ( Traversable f
          , MapKey (ref (Maybe (Term f ref)))
          , MonadError (UnificationException f ref) m
          , MonadRef ref m
          ) => Term f ref -> m (f (Fix f))
freeze = liftM getFix . freeze'
  where
    freeze' =
      flip evalStateT Map.empty . loop
    loop =
      semiprune >=> loop'
    loop' (Semipruned _ (UnboundVarS r)) =
      throwError $ UnboundVar r
    loop' (Semipruned _ (BoundVarS r f)) = localState $ do
      r `seenAs` f
      liftM Fix . mapM loop $ f
    loop' (Semipruned _ (TermS f)) =
      liftM Fix . mapM loop $ f

unfreeze :: Functor f => f (Fix f) -> Term f ref
unfreeze = Free . fmap (unfreeze . getFix)

seenAs :: ( MapKey (ref (Maybe (Term f ref)))
          , MonadError (UnificationException f ref) m
          ) =>
          ref (Maybe (Term f ref)) ->
          f (Term f ref) ->
          StateT (S f ref) m ()
seenAs r0 f0 = do
  s <- get
  maybe
    (put $! Map.insert r0 f0 s)
    (\ f -> throwError $ r0 `OccursIn` f) $
    Map.lookup r0 s

type S f ref = Map (ref (Maybe (Term f ref))) (f (Term f ref))

localState :: MonadState s m => m a -> m a
localState m = do
  s <- get
  a <- m
  put s
  return a

bindM2 :: Monad m => m a -> m b -> (a -> b -> m c) -> m c
bindM2 a b f = do
  a' <- a
  b' <- b
  f a' b'