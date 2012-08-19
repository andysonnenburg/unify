{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , GADTs
  , ViewPatterns #-}
module Language.HM.DM.InferType
       ( inferType
       ) where

import Control.Applicative
import Control.Category ((<<<))
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
import Data.Traversable

import Language.HM.DM.Exp (Church, Curry, Exp)
import qualified Language.HM.DM.Exp as E
import Language.HM.DM.Type (Mono)
import qualified Language.HM.DM.Type as T
import Language.HM.Var

import Prelude hiding (foldl, mapM)

inferType :: ( Eq (name Value)
             , Hashable (name Value)
             , Eq (name Type)
             , Hashable (name Type)
             , MonadName name m
             , MonadUnify (T.Mono name) ref m
             ) =>
             Fix (Exp Curry name (Fix (Mono name))) ->
             m (Fix (Exp Church name (Fix (Mono name)))) -- ^
inferType = inferType'
  where
    inferType' t =
      unwrapMonadT <<<
      flip runReaderT Map.empty $
      freezeExp =<< do
        expected <- pure <$> newFreeVar
        Fix . fst <$> gen (getFix t) expected
    asCurry :: Fix (Exp Curry name mono) -> Fix (Exp Curry name mono)
    asCurry = id
    loop t@(E.Lit i) expected = do
      let _ = asCurry $ Fix t
      _ <- unify expected (wrap T.Int)
      return $ E.Lit i
    loop (E.Var x) rho = do
      sigma <- lookupPoly x
      f <- inst sigma rho
      return $ f $ E.Var x
    loop (E.Abs x t) expected = do
      tau <- pure <$> newFreeVar
      rho <- pure <$> newFreeVar
      t' <- insertMono x tau $ Fix <$> loop (getFix t) rho
      _ <- unify expected (wrap $ T.Fn tau rho)
      return $ E.Abs (x, T.Forall mempty tau) t'
    loop (E.AAbs (x, unfreeze -> tau) t) expected = do
      tau' <- pure <$> newFreeVar
      rho <- pure <$> newFreeVar
      t' <- insertMono x tau' $ Fix <$> loop (getFix t) rho
      skol (T.Forall mempty tau') (T.Forall mempty tau)
      _ <- unify expected (wrap $ T.Fn tau rho)
      return $ E.Abs (x, T.Forall mempty tau) t'
    loop (E.App t u) rho = do
      tau' <- pure <$> newFreeVar
      t' <- Fix <$> loop (getFix t) tau'
      tau <- pure <$> newFreeVar
      u' <- Fix <$> loop (getFix u) tau
      _ <- unify tau' (wrap $ T.Fn tau rho)
      return $ E.App t' u'
    loop (E.Let x u t) rho = do
      tau <- pure <$> newFreeVar
      (u', sigma) <- gen (getFix u) tau
      t' <- insertPoly x sigma $ Fix <$> loop (getFix t) rho
      return $ E.Let (x, sigma) (Fix u') t'
    loop (E.Ann t (fmap unfreeze -> sigma)) rho = do
      rho' <- pure <$> newFreeVar
      (t', sigma') <- gen (getFix t) rho'
      skol sigma' sigma
      f <- inst sigma rho
      return $ f t'

    gen t rho = do
      t' <- loop t rho
      gamma <- asks $ fmap getMono
      a <- freezeVars . Set.toList =<< (\\) <$> ftv rho <*> ftv' gamma      
      return (E.TyAbs a (Fix t'), T.Forall a rho)
      where
        freezeVars = mapM $ \ freeVar -> do
          a <- newTypeVar
          _ <- unify (pure freeVar) (wrap $ T.Var a)
          return a
        (\\) = Set.difference

    inst (T.Forall as rho) expected = do
      taus <- mapM (const $ pure <$> newFreeVar) as
      let bindings = Map.fromList $ zip as taus
      rho' <- flip rewrite rho $ \ f ->
        case f of
          Free (T.Var a) -> Map.lookup a bindings
          _ -> Nothing
      _ <- unify expected rho'
      return $ \ x -> E.TyApp (Fix x) bindings

    skol sigma (T.Forall _a rho) = spec sigma rho

    spec sigma rho2 = do
      rho1 <- pure <$> newFreeVar
      _ <- inst sigma rho1
      mono rho1 rho2

    mono tau tau' = void $ unify tau tau'

    lookupPoly x = asks (!x)

    insertPoly x sigma = local $ Map.insert x sigma

    insertMono x tau = local $ Map.insert x (T.Forall mempty tau)

    getMono (T.Forall _ rho) = rho

    freezePoly (T.Forall a rho) = T.Forall a <$> freeze rho

    freezeExp = fmap Fix . freezeExp' . getFix
      where
        freezeExp' (E.Lit i) =
          return $ E.Lit i
        freezeExp' (E.Var x) =
          return $ E.Var x
        freezeExp' (E.Abs (x, sigma) t) = do
          sigma' <- freezePoly sigma
          t' <- freezeExp t
          return $ E.Abs (x, sigma') t'
        freezeExp' (E.TyAbs a t) =
          E.TyAbs a <$> freezeExp t
        freezeExp' (E.App t u) =
          E.App <$> freezeExp t <*> freezeExp u
        freezeExp' (E.TyApp e t) =
          E.TyApp <$> freezeExp e <*> mapM freeze t
        freezeExp' (E.Let (x, sigma) u t) = do
          sigma' <- freezePoly sigma
          u' <- freezeExp u
          t' <- freezeExp t
          return $ E.Let (x, sigma') u' t'

    ftv m = do
      ms <- universe m
      return $ Set.fromList [a | Pure a <- ms]

    ftv' ms = do
      ms' <- universeBi ms
      return $ Set.fromList [a | Pure a <- ms']
