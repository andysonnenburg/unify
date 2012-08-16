{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , StandaloneDeriving
  , TypeFamilies
  , UndecidableInstances #-}
{-# OPTIONS_GHC
    -fno-warn-missing-signatures
    -fno-warn-name-shadowing #-}
module Control.Monad.Unify
       ( module Exports
       , Var
       , Term
       , Unifiable (..)
       , UnificationError (..)
       , MonadUnify
       , unify
       , newFreeVar
       , universe
       , rewrite
       , rewriteM
       , universeBi
       , freeze
       , unfreeze
       ) where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Free as Exports
import Control.Monad.Reader
import Control.Monad.Ref.Class
import Control.Monad.State
import Control.Monad.Wrap
import Control.Monad.Writer

import Data.Fix
import Data.Foldable
import Data.Functor.Identity
import Data.Hashable
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set
import Data.Traversable

newtype Var f ref = Var (ref (Maybe (Term f ref)))
deriving instance Eq (ref (Maybe (Term f ref))) => Eq (Var f ref)
deriving instance Ord (ref (Maybe (Term f ref))) => Ord (Var f ref)
deriving instance Show (ref (Maybe (Term f ref))) => Show (Var f ref)

instance Hashable (ref (Maybe (Term f ref))) => Hashable (Var f ref) where
  hash (Var x) = hash x
  hashWithSalt salt (Var x) = hashWithSalt salt x

type Term f ref = Free f (Var f ref)

data UnificationError f ref
  = OccursIn (Var f ref) (f (Term f ref))
  | TermMismatch (f (Term f ref)) (f (Term f ref))
  | UnboundVar (Var f ref)

deriving instance ( Show (f (Term f ref))
                  , Show (ref (Maybe (Term f ref)))
                  ) => Show (UnificationError f ref)

class Traversable f => Unifiable f where
  zipMatch :: f a -> f b -> Maybe (f (a, b))

type MonadUnify f ref m = ( Unifiable f
                          , Eq (ref (Maybe (Term f ref)))
                          , Hashable (ref (Maybe (Term f ref)))
                          , MonadError (UnificationError f ref) m
                          , MonadRef ref m
                          ) -- ^

unify :: MonadUnify f ref m => Term f ref -> Term f ref -> m (Term f ref) -- ^
unify = unify'
  where
    unify' t1 t2 = unwrapMonadT $ runReaderT (loop t1 t2) Map.empty
    loop t1 t2 = bindM2 (semiprune t1) (semiprune t2) loop'
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
        writeRef r2 . Just =<<
          (r1 `mustNotOccurIn` f1 $
           r2 `mustNotOccurIn` f2 $
           match f1 f2)
        writeRef r1 $ Just t2
        return t2
    loop' (S t1 (UnboundVarS r1)) (S t2 (TermS _)) = do
      writeRef r1 $ Just t2
      return t1
    loop' (S t1 (TermS _)) (S t2 (UnboundVarS r2)) = do
      writeRef r2 $ Just t1
      return t2
    loop' (S t1 (BoundVarS r1 f1)) (S _ (TermS f2)) = do
      writeRef r1 . Just =<<
        (r1 `mustNotOccurIn` f1 $
         match f1 f2)
      return t1
    loop' (S _ (TermS f1)) (S t2 (BoundVarS r2 f2)) = do
      writeRef r2 . Just =<<
        (r2 `mustNotOccurIn` f2 $
         match f1 f2)
      return t2
    loop' (S _ (TermS f1)) (S _ (TermS f2)) =
      match f1 f2
    match x y =
      maybe
      (throwError $ TermMismatch x y)
      (fmap Free . traverse (uncurry loop)) $
      zipMatch x y
    (r `mustNotOccurIn` f) m =
      maybe
      (local (Map.insert r f) m)
      (\ f' -> throwError $ Var r `OccursIn` f') =<<
      asks (Map.lookup r)

data TermS f ref
  = S !(Term f ref) !(Semipruned f ref)

data Semipruned f ref
  = UnboundVarS !(ref (Maybe (Term f ref)))
  | BoundVarS !(ref (Maybe (Term f ref))) !(f (Term f ref))
  | TermS !(f (Term f ref))

semiprune :: MonadRef ref m => Term f ref -> m (TermS f ref)
semiprune = semiprune'
  where
    semiprune' t0@(Pure (Var r0)) = loop t0 r0
    semiprune' t0@(Free f0) = return $ S t0 (TermS f0)
    loop t0 r0 = readRef r0 >>= loop'
      where
        loop' Nothing =
          return $ S t0 (UnboundVarS r0)
        loop' (Just t1@(Pure (Var r1))) = do
          sp@(S t _) <- loop t1 r1
          writeRef r0 $ Just t
          return sp
        loop' (Just (Free f)) =
          return $ S t0 (BoundVarS r0 f)

newFreeVar :: MonadRef ref m => m (Var f ref)
newFreeVar = unwrapMonadT $ Var <$> newRef Nothing

universe :: ( Foldable f
            , Eq (ref (Maybe (Term f ref)))
            , Hashable (ref (Maybe (Term f ref)))
            , MonadRef ref m
            ) => Term f ref -> m [Term f ref]
universe = universeBi . Identity

universeBi :: ( Foldable f
              , Foldable w
              , Eq (ref (Maybe (Term w ref)))
              , Hashable (ref (Maybe (Term w ref)))
              , MonadRef ref m
              ) => f (Term w ref) -> m [Term w ref]
universeBi =
  flip evalStateT Set.empty .
  foldlM loop []
  where
    loop a = loop' a <=< semiprune
    loop' a (S t (UnboundVarS r)) =
      ifM (hasSeen r)
      (return a) $ do
        seen r
        return $ t:a
    loop' a (S _ (BoundVarS r f)) =
      ifM (hasSeen r)
      (return a) $ do
        seen r
        foldlM loop a f
    loop' a (S t (TermS f)) =
      liftM (t:) $ foldlM loop a f
    hasSeen r = gets $ Set.member r
    seen r = modify $ Set.insert r
    ifM m x y = do
      p <- m
      if p then x else y

rewrite :: ( Traversable f
           , Eq (ref (Maybe (Term f ref)))
           , Hashable (ref (Maybe (Term f ref)))
           , MonadError (UnificationError f ref) m
           , MonadRef ref m
           ) =>
           (Term f ref -> Maybe (Term f ref)) ->
           Term f ref -> m (Term f ref) -- ^
rewrite = rewriteM . (return .)

rewriteM :: ( Traversable f
            , Eq (ref (Maybe (Term f ref)))
            , Hashable (ref (Maybe (Term f ref)))
            , MonadError (UnificationError f ref) m
            , MonadRef ref m
            ) =>
            (Term f ref -> m (Maybe (Term f ref))) ->
            Term f ref -> m (Term f ref) -- ^
rewriteM f =
  unwrapMonadT . flip evalStateT Map.empty . evalWriterT . loop
  where
    loop = loop' <=< semiprune
    loop' (S t (UnboundVarS r)) = r `whenUnseen` do
      t' <- g t
      r `seenAs` t'
      return t'
    loop' (S t (BoundVarS r f)) = r `whenUnseen` do
      r `mustNotOccurIn` f
      (t', changed) <- listen $ g =<< wrap <$> traverse loop f
      let t'' = if getAny changed then t' else t
      r `seenAs` t''
      return t''
    loop' (S t (TermS f)) = do
      (f', changed) <- listen $ traverse loop f
      g $ if getAny changed then wrap f' else t
    g t = whenJust t $ \ t' -> do
      tellChanged
      g' t'
    g' = flip whenJust g'
    whenJust t m = maybe (return t) m =<< f' t
    f' = lift . lift . lift . f
    tellChanged = tell $ Any True
    evalWriterT = fmap fst . runWriterT

freeze :: ( Traversable f
          , Eq (ref (Maybe (Term f ref)))
          , Hashable (ref (Maybe (Term f ref)))
          , MonadError (UnificationError f ref) m
          , MonadRef ref m
          ) => Term f ref -> m (Fix f) -- ^
freeze =
  unwrapMonadT . flip evalStateT Map.empty . loop
  where
    loop =
      semiprune >=> loop'
    loop' (S _ (UnboundVarS r)) =
      throwError $ UnboundVar $ Var r
    loop' (S _ (BoundVarS r f)) =
      r `whenUnseen` do
        r `mustNotOccurIn` f
        f' <- Fix <$> traverse loop f
        r `seenAs` f'
        return f'
    loop' (S _ (TermS f)) =
      Fix <$> traverse loop f

r `whenUnseen` m = do
  s <- get
  case Map.lookup r s of
    Nothing -> m
    Just (Left f) -> throwError $ Var r `OccursIn` f
    Just (Right f) -> return f

r `mustNotOccurIn` f =
  modify $ Map.insert r (Left f)

r `seenAs` f =
  modify $ Map.insert r (Right f)

unfreeze :: Functor f => Fix f -> Term f ref
unfreeze = Free . fmap unfreeze . getFix

bindM2 :: Monad m => m a -> m b -> (a -> b -> m c) -> m c
bindM2 a b f = do
  a' <- a
  b' <- b
  f a' b'
