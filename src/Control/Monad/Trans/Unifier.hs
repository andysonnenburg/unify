{-# LANGUAGE
    FlexibleContexts
  , StandaloneDeriving
  , UndecidableInstances
  , ViewPatterns #-}
module Control.Monad.Trans.Unifier
       ( module Exports
       , UnifierT
       , runUnifierT
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
import Data.Functor.Plus
import Data.Monoid
import Data.Traversable

import Control.Monad.Trans.Unifier.Either hiding (Either)
import qualified Control.Monad.Trans.Unifier.Map as Map
import qualified Control.Monad.Trans.Unifier.Set as Set

import Prelude hiding (mapM)

type UnifierT f ref m = EitherT (UnificationException f ref) m

runUnifierT :: MonadRef ref m =>
               UnifierT f ref m a ->
               m (Either (UnificationException f ref) a)
runUnifierT = runEitherT

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

class Traversable f => Unifiable f where
  zipMatch :: f a -> f b -> Maybe (f (a, b))

deriving instance Show (ref (Maybe (Term f ref))) => Show (Var f ref)

freshTerm :: MonadRef ref m => m (Term f ref)
freshTerm = liftM (Pure . Var) $ newRef Nothing

unify :: ( Unifiable f
         , Eq (ref (Maybe (Term f ref)))
         , Map.Key (ref (Maybe (Term f ref)))
         , MonadError (UnificationException f ref) m
         , MonadRef ref m
         ) => Term f ref -> Term f ref -> m (Term f ref)
unify = unify'
  where
    unify' t1 t2 =
      evalStateT (loop t1 t2) zero
    loop t1 t2 =
      bindM2 (semiprune t1) (semiprune t2) loop'
    loop' (S _ (UnboundVarS r1)) (S t2 (UnboundVarS r2))
      | r1 == r2 =
        return t2
      | otherwise = do
        writeRef r1 $ Just t2
        return t2
    loop' (S _ (UnboundVarS r1)) (S t2 (BoundVarS _ _)) = do
      writeRef r1 $ Just t2
      return t2
    loop' (S t1 (BoundVarS _ _)) (S _ (UnboundVarS r2)) = do
      writeRef r2 $ Just t1
      return t1
    loop' (S _ (BoundVarS r1 f1)) (S t2 (BoundVarS r2 f2))
      | r1 == r2 =
        return t2
      | otherwise = do
        writeRef r2 . Just <=< localState $ do
          r1 `seenAs` f1
          r2 `seenAs` f2
          match f1 f2
        writeRef r1 $ Just t2
        return t2
    loop' (S t1 (UnboundVarS r1)) (S t2 (TermS _)) = do
      writeRef r1 $ Just t2
      return t1
    loop' (S t1 (TermS _)) (S t2 (UnboundVarS r2)) = do
      writeRef r2 $ Just t1
      return t2
    loop' (S t1 (BoundVarS r1 f1)) (S _ (TermS f2)) = do
      writeRef r1 . Just <=< localState $ do
        r1 `seenAs` f1
        match f1 f2
      return t1
    loop' (S _ (TermS f1)) (S t2 (BoundVarS r2 f2)) = do
      writeRef r2 . Just <=< localState $ do
        r2 `seenAs` f2
        match f1 f2
      return t2
    loop' (S _ (TermS f1)) (S _ (TermS f2)) =
      match f1 f2
    match x y =
      maybe
      (throwError $ TermMismatch x y)
      (liftM Free . mapM (uncurry loop)) $
      zipMatch x y
    seenAs r0 f0 = do
      s <- get
      maybe
        (put $! Map.insert r0 f0 s)
        (\ f -> throwError $ r0 `OccursIn` f) $
        Map.lookup r0 s

data TermS f ref
  = S !(Term f ref) !(Semipruned f ref)

data Semipruned f ref
  = UnboundVarS !(ref (Maybe (Term f ref)))
  | BoundVarS !(ref (Maybe (Term f ref))) !(f (Term f ref))
  | TermS !(f (Term f ref))

semiprune :: MonadRef ref m => Term f ref -> m (TermS f ref)
semiprune = semiprune'
  where
    semiprune' t0@(Pure (getRef -> r0)) =
      loop t0 r0
    semiprune' t0@(Free f0) =
      return $ S t0 (TermS f0)
    loop t0 r0 =
      readRef r0 >>=
      maybe
      (return $ S t0 (UnboundVarS r0))
      (\ t -> case t of
          Pure (getRef -> r) -> do
            sp@(S t' _) <- loop t r
            writeRef r0 $ Just t'
            return sp
          Free f ->
            return $ S t0 (BoundVarS r0 f))

freeVars :: ( Foldable f
            , Set.Elem (ref (Maybe (Term f ref)))
            , MonadRef ref m
            ) => Term f ref -> m [ref (Maybe (Term f ref))]
freeVars =
  liftM Set.toList .
  foldlUnboundVarsM (\ a -> return . flip Set.insert a) mempty

foldlUnboundVarsM :: ( Foldable f
                     , Set.Elem (ref (Maybe (Term f ref)))
                     , MonadRef ref m
                     ) => (a -> ref (Maybe (Term f ref)) -> m a) -> a -> Term f ref -> m a
foldlUnboundVarsM k a0 =
  flip evalStateT mempty . loop a0
  where
    loop a =
      semiprune >=> loop' a
    loop' a (S _ (UnboundVarS r)) =
      lift $ k a r
    loop' a (S _ (BoundVarS r f)) =
      ifM (hasSeen r)
      (return a) $ do
        seen r
        foldlM loop a f
    loop' a (S _ (TermS f)) =
      foldlM loop a f
    hasSeen r =
      gets $ Set.member r
    seen r =
      modify $ Set.insert r
    ifM m x y = do
      p <- m
      if p then x else y

freeze :: ( Traversable f
          , Map.Key (ref (Maybe (Term f ref)))
          , MonadError (UnificationException f ref) m
          , MonadRef ref m
          ) => Term f ref -> m (f (Fix f))
freeze = liftM getFix . freeze'
  where
    freeze' =
      flip evalStateT zero . loop
    loop =
      semiprune >=> loop'
    loop' (S _ (UnboundVarS r)) =
      throwError $ UnboundVar r
    loop' (S _ (BoundVarS r f)) =
      whenUnseen r f $ do
        seen r
        f' <- liftM Fix . mapM loop $ f
        r `seenAs` f'
        return f'
    loop' (S _ (TermS f)) =
      liftM Fix . mapM loop $ f
    whenUnseen r f m = do
      s <- get
      case Map.lookup r s of
        Nothing -> m
        Just Nothing -> throwError $ r `OccursIn` f
        Just (Just f') -> return f'
    seen r =
      modify $ Map.insert r Nothing
    r `seenAs` f' =
      modify $ Map.insert r (Just f')

unfreeze :: Functor f => f (Fix f) -> Term f ref
unfreeze = Free . fmap (unfreeze . getFix)

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
