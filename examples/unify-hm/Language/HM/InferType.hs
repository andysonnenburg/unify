{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , FlexibleContexts
  , GADTs
  , LambdaCase
  , ViewPatterns #-}
module Language.HM.InferType
       ( inferType
       ) where

import Control.Applicative
import Control.Category
import Control.Comonad.Cofree
import Control.Monad.Name.Class
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Unify
import Control.Monad.Wrap hiding (mapM)

import Data.Fix
import Data.Hashable
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set
import Data.Monoid
import Data.Foldable
import Data.Traversable

import Language.HM.Exp (Style (..), Exp)
import qualified Language.HM.Exp as E
import Language.HM.Type (Mono)
import qualified Language.HM.Type as T
import Language.HM.Var

import Prelude hiding ((.), foldl, id, mapM)

inferType :: ( Eq (name Value)
             , Eq (name Type)
             , Hashable (name Value)
             , Hashable (name Type)
             , MonadName name m
             , MonadUnify (T.Mono name) ref m
             ) =>
             Cofree (Exp Curry name (Fix (Mono name))) a ->
             m (Cofree (Exp Church name (Fix (Mono name))) a) -- ^
inferType = inferType'
  where
    inferType' t =
      unwrapMonadT <<<
      flip runReaderT Map.empty $
      freezeExp =<< do
        expected <- pure <$> newFreeVar
        fst <$> gen t expected
    asCurry :: Cofree (Exp Curry name mono) a -> Cofree (Exp Curry name mono) a
    asCurry = id
    asChurch :: Cofree (Exp Church name mono) a -> Cofree (Exp Church name mono) a
    asChurch = id
    loop (asCurry -> a :< f) expected = case f of
      E.Lit i -> do
        unify' expected (wrap T.Int)
        return $ a :<  E.Lit i
      E.Var x -> do
        sigma <- lookupPoly x
        f <- inst sigma expected
        return $ f $ a :< E.Var x
      E.Abs x t -> do
        tau <- pure <$> newFreeVar
        rho <- pure <$> newFreeVar
        t' <- insertMono x tau $ loop t rho
        unify' expected (wrap $ T.Fn tau rho)
        return $ a :< E.Abs (x, T.Forall mempty tau) t'
      E.AAbs (x, unfreeze -> tau) t -> do
        rho <- pure <$> newFreeVar
        t' <- insertMono x tau $ loop t rho
        unify' expected (wrap $ T.Fn tau rho)
        return $ a :< E.Abs (x, T.Forall mempty tau) t'
      E.App t u -> do
        tau' <- pure <$> newFreeVar
        t' <- loop t tau'
        tau <- pure <$> newFreeVar
        u' <- loop u tau
        unify' tau' (wrap $ T.Fn tau expected)
        return $ a :< E.App t' u'
      E.Let x u t -> do
        tau <- pure <$> newFreeVar
        (u', sigma) <- gen u tau
        t' <- insertPoly x sigma $ loop t expected
        return $ a :< E.Let (x, sigma) u' t'
      E.Ann t (fmap unfreeze -> sigma) -> do
        rho' <- pure <$> newFreeVar
        (t', sigma') <- gen t rho'
        skol sigma' sigma
        f <- inst sigma expected
        return $ f t'

    gen t rho = do
      t' <- loop t rho
      gamma <- asks $ fmap getMono
      a <- freezeVars =<< (\\) <$> ftv rho <*> ftv' gamma
      return (using t' $ E.TyAbs a t', T.Forall a rho)
      where
        freezeVars = mapM' $ \ freeVar -> do
          a <- newTypeVar
          unify' (pure freeVar) (wrap $ T.Var a)
          return a
        mapM' f = foldlM g Set.empty
          where
            g a b = Set.insert <$> f b <*> pure a
        (\\) = Set.difference

    inst (T.Forall (Set.toList -> as) rho) expected = do
      taus <- mapM (const $ pure <$> newFreeVar) as
      let bindings = Map.fromList $ zip as taus
      rho' <- flip rewrite rho $ \ f ->
        case f of
          Free (T.Var a) -> Map.lookup a bindings
          _ -> Nothing
      unify' expected rho'
      return $ \ e -> using e $ E.TyApp e bindings

    skol sigma (T.Forall _a rho) = spec sigma rho

    spec sigma rho2 = do
      rho1 <- pure <$> newFreeVar
      void $ inst sigma rho1
      mono rho1 rho2

    mono tau tau' = unify' tau tau'

    lookupPoly x = asks (!x)

    insertPoly x sigma = local $ Map.insert x sigma

    insertMono x tau = local $ Map.insert x (T.Forall mempty tau)

    getMono (T.Forall _ rho) = rho

    freezePoly (T.Forall a rho) = T.Forall a <$> freeze rho

    freezeExp = mapM' $ \ case
      E.Lit i ->
        return $ E.Lit i
      E.Var x ->
        return $ E.Var x
      E.Abs (x, sigma) t -> do
        sigma' <- freezePoly sigma
        t' <- freezeExp t
        return $ E.Abs (x, sigma') t'
      E.TyAbs a t ->
        E.TyAbs a <$> freezeExp t
      E.App t u ->
        E.App <$> freezeExp t <*> freezeExp u
      E.TyApp e t ->
        E.TyApp <$> freezeExp e <*> mapM freeze t
      E.Let (x, sigma) u t -> do
        sigma' <- freezePoly sigma
        u' <- freezeExp u
        t' <- freezeExp t
        return $ E.Let (x, sigma') u' t'
      where
        mapM' f (asChurch -> a :< b) = (a :<) <$> f b

    using (a :< _) b = a :< b

    ftv m = do
      ms <- universe m
      return $ Set.fromList [a | Pure a <- ms]

    ftv' ms = do
      ms' <- universeBi ms
      return $ Set.fromList [a | Pure a <- ms']

    unify' a b = void $ mapE (, a) $ unify a b