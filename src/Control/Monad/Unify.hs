{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
  , StandaloneDeriving
  , UndecidableInstances #-}
{-# OPTIONS_GHC
    -fno-warn-missing-signatures #-}
module Control.Monad.Unify
       ( Term (..)
       , term
       , Unifiable (..)
       , UnificationError (..)
       , unify
       , freshTerm
       , universe
       , rewrite
       , rewriteM
       , universeBi
       , freeze
       , unfreeze
       ) where

import Control.Applicative
import Control.Monad.Catch.Class
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

data Term f ref
  = Var (ref (Maybe (Term f ref)))
  | Term (f (Term f ref))

term :: f (Term f ref) -> Term f ref
term = Term

data UnificationError f ref
  = ref (Maybe (Term f ref)) `OccursIn` f (Term f ref)
  | f (Term f ref) `DoesNotMatch` f (Term f ref)
  | UnboundVar (ref (Maybe (Term f ref)))

deriving instance ( Show (f (Term f ref))
                  , Show (ref (Maybe (Term f ref)))
                  ) => Show (UnificationError f ref)

class Traversable f => Unifiable f where
  zipMatch :: f a -> f b -> Maybe (f (a, b))

unify :: ( Unifiable f
         , Eq (ref (Maybe (Term f ref)))
         , Hashable (ref (Maybe (Term f ref)))
         , MonadRef ref m
         , MonadThrow (UnificationError f ref) m
         ) => Term f ref -> Term f ref -> m (Term f ref) -- ^
unify = unify'
  where
    unify' t1 t2 = unwrapMonadT $ runReaderT (loop t1 t2) Map.empty
    loop = flip onM semiprune $ curry $ \ case
      (S _ (UnboundVarS r1), S t2 (UnboundVarS r2))
        | r1 == r2 ->
          return t2
        | otherwise -> do
          writeRef r1 $ Just t2
          return t2
      (S _ (UnboundVarS r1), S t2 (BoundVarS _ _)) -> do
        writeRef r1 $ Just t2
        return t2
      (S t1 (BoundVarS _ _), S _ (UnboundVarS r2)) -> do
        writeRef r2 $ Just t1
        return t1
      (S _ (BoundVarS r1 f1), S t2 (BoundVarS r2 f2))
        | r1 == r2 ->
          return t2
        | otherwise -> do
          writeRef r2 . Just =<<
            (r1 `localMustNotOccurIn` f1 $
             r2 `localMustNotOccurIn` f2 $
             match f1 f2)
          writeRef r1 $ Just t2
          return t2
      (S t1 (UnboundVarS r1), S t2 (TermS _)) -> do
        writeRef r1 $ Just t2
        return t1
      (S t1 (TermS _), S t2 (UnboundVarS r2)) -> do
        writeRef r2 $ Just t1
        return t2
      (S t1 (BoundVarS r1 f1), S _ (TermS f2)) -> do
        writeRef r1 . Just =<<
          (r1 `localMustNotOccurIn` f1 $
           match f1 f2)
        return t1
      (S _ (TermS f1), S t2 (BoundVarS r2 f2)) -> do
        writeRef r2 . Just =<<
          (r2 `localMustNotOccurIn` f2 $
           match f1 f2)
        return t2
      (S _ (TermS f1), S _ (TermS f2)) ->
        match f1 f2
    match x y =
      maybe
      (throw $ x `DoesNotMatch` y)
      (fmap term . traverse (uncurry loop)) $
      zipMatch x y
    (r `localMustNotOccurIn` f) m =
      maybe
      (local (Map.insert r f) m)
      (throw . (r `OccursIn`)) =<<
      asks (Map.lookup r)

data TermS f ref
  = S !(Term f ref) !(Semipruned f ref)

data Semipruned f ref
  = UnboundVarS !(ref (Maybe (Term f ref)))
  | BoundVarS !(ref (Maybe (Term f ref))) !(f (Term f ref))
  | TermS !(f (Term f ref))

semiprune :: MonadRef ref m => Term f ref -> m (TermS f ref)
semiprune t0@(Term f0) = return $ S t0 (TermS f0)
semiprune t0@(Var r0) = loop t0 r0
  where
    loop t r = readRef r >>= \ case
      Nothing ->
        return $ S t (UnboundVarS r)
      Just t'@(Var r') -> do
        s@(S t'' _) <- loop t' r'
        writeRef r $ Just t''
        return s
      Just (Term f) ->
        return $ S t (BoundVarS r f)

freshTerm :: MonadRef ref m => m (Term f ref)
freshTerm = unwrapMonadT $ Var <$> newRef Nothing

universe :: ( Foldable f
            , Eq (ref (Maybe (Term f ref)))
            , Hashable (ref (Maybe (Term f ref)))
            , MonadRef ref m
            ) => Term f ref -> m [Term f ref]
universe = universeBi . Identity

universeBi :: ( Foldable f
              , Foldable u
              , Eq (ref (Maybe (Term u ref)))
              , Hashable (ref (Maybe (Term u ref)))
              , MonadRef ref m
              ) => f (Term u ref) -> m [Term u ref]
universeBi =
  flip evalStateT Set.empty .
  foldlM loop []
  where
    loop a = semiprune >=> \ case
      S t (UnboundVarS r) -> ifM (hasSeen r) (return a) $ do
        seen r
        return $ t:a
      S _ (BoundVarS r f) -> ifM (hasSeen r) (return a) $ do
        seen r
        foldlM loop a f
      S t (TermS f) ->
        liftM (t:) $ foldlM loop a f
    hasSeen r = gets $ Set.member r
    seen r = modify $ Set.insert r
    ifM m x y = do
      p <- m
      if p then x else y

rewrite :: ( Traversable f
           , Eq (ref (Maybe (Term f ref)))
           , Hashable (ref (Maybe (Term f ref)))
           , MonadRef ref m
           , MonadThrow (UnificationError f ref) m
           ) =>
           (Term f ref -> Maybe (Term f ref)) ->
           Term f ref -> m (Term f ref) -- ^
rewrite = rewriteM . (return .)

rewriteM :: ( Traversable f
            , Eq (ref (Maybe (Term f ref)))
            , Hashable (ref (Maybe (Term f ref)))
            , MonadRef ref m
            , MonadThrow (UnificationError f ref) m
            ) =>
            (Term f ref -> m (Maybe (Term f ref))) ->
            Term f ref -> m (Term f ref) -- ^
rewriteM f =
  unwrapMonadT . flip evalStateT Map.empty . evalWriterT . loop
  where
    loop = semiprune >=> \ case
      S t (UnboundVarS r) -> whenUnseen r $ do
        t' <- g t
        r `seenAs` t'
        return t'
      S t (BoundVarS r x) -> whenUnseen r $ do
        r `mustNotOccurIn` x
        (t', changed) <- listen $ g =<< term <$> traverse loop x
        let t'' = if getAny changed then t' else t
        r `seenAs` t''
        return t''
      S t (TermS x) -> do
        (x', changed) <- listen $ traverse loop x
        g $ if getAny changed then term x' else t
    g t = whenJust f' t $ \ t' -> do
      tellChanged
      unfoldLastM f' t'
    whenJust p x k = maybe (return x) k =<< p x
    unfoldLastM k x = maybe (return x) (unfoldLastM k) =<< k x
    f' = lift . lift . lift . f
    tellChanged = tell $ Any True
    evalWriterT = fmap fst . runWriterT

freeze :: ( Traversable f
          , Eq (ref (Maybe (Term f ref)))
          , Hashable (ref (Maybe (Term f ref)))
          , MonadRef ref m
          , MonadThrow (UnificationError f ref) m
          ) => Term f ref -> m (Fix f) -- ^
freeze =
  unwrapMonadT . flip evalStateT Map.empty . loop
  where
    loop = semiprune >=> \ case
      S _ (UnboundVarS r) ->
        throw $ UnboundVar r
      S _ (BoundVarS r f) -> whenUnseen r $ do
        r `mustNotOccurIn` f
        f' <- Fix <$> traverse loop f
        r `seenAs` f'
        return f'
      S _ (TermS f) ->
        Fix <$> traverse loop f

whenUnseen r m = do
  s <- get
  case Map.lookup r s of
    Nothing -> m
    Just (Left f) -> throw $ r `OccursIn` f
    Just (Right f) -> return f

r `mustNotOccurIn` f =
  modify $ Map.insert r (Left f)

r `seenAs` f =
  modify $ Map.insert r (Right f)

unfreeze :: Functor f => Fix f -> Term f ref
unfreeze = Term . fmap unfreeze . getFix

onM :: Monad m => (b -> b -> m c) -> (a -> m b) -> a -> a -> m c
onM g f x y = bind2 (f x) (f y) g

bind2 :: Monad m => m a -> m b -> (a -> b -> m c) -> m c
bind2 a b f = do
  a' <- a
  b' <- b
  f a' b'
