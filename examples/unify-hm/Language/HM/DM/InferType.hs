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
import Control.Monad.Ident.Class
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Unify
import Control.Monad.Wrap hiding (mapM)

import Data.Fix
import Data.Foldable
import Data.Hashable
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set

import Language.HM.DM.Exp (Exp, I)
import qualified Language.HM.DM.Exp as E
import Language.HM.DM.Type (Mono, Poly)
import qualified Language.HM.DM.Type as T
import Language.HM.Var

import Prelude hiding (mapM)

inferType :: ( Eq (name Value)
             , Hashable (name Value)
             , Eq (name Type)
             , Hashable (name Type)
             , MonadIdent name m
             , MonadUnify (T.Mono name) ref m
             ) =>
             Fix (Exp I name (Fix (Mono name))) ->
             m (Poly name (Fix (Mono name))) -- ^
inferType =
  unwrapMonadT <<<
  flip runReaderT Map.empty <<<
  freezePoly <=<
  gen
  where
    loop =
      loop' . getFix
    loop' (E.Lit _) =
      return $ wrap T.Int
    loop' (E.Var x) = do
      sigma <- lookupPoly x
      inst sigma
    loop' (E.Abs x t) = do
      tau <- pure <$> newFreeVar
      rho <- insertMono x tau $ loop t
      return $ wrap $ T.Fn tau rho
    loop' (E.AAbs (x, unfreeze -> tau) t) = do
      tau' <- pure <$> newFreeVar
      rho <- insertMono x tau' $ loop t
      skol (T.Forall Set.empty tau') (T.Forall Set.empty tau)
      return $ wrap $ T.Fn tau rho
    loop' (E.App t u) = do
      tau' <- loop t
      tau <- loop u
      rho <- pure <$> newFreeVar
      _ <- unify tau' (wrap $ T.Fn tau rho)
      return rho
    loop' (E.Let x u t) = do
      sigma <- gen u
      insertPoly x sigma $ loop t
    loop' (E.Ann t (fmap unfreeze -> sigma)) = do
      sigma' <- gen t
      skol sigma' sigma
      inst sigma

    gen t = do
      rho <- loop t
      gamma <- asks $ fmap getMono
      a <- freezeVars =<< (\\) <$> getFreeVars rho <*> getAllFreeVars gamma
      return $ T.Forall a rho
      where
        freezeVars = mapM $ \ freeVar -> do
          a <- newTypeVar
          _ <- unify (pure freeVar) (wrap $ T.Var a)
          return a
        mapM f =
          foldlM (\ a -> fmap (flip Set.insert a) . f) Set.empty
        (\\) =
          Set.difference

    inst (T.Forall as rho) = do
      taus <- zipM (pure <$> newFreeVar) as
      flip rewrite rho $ \ f ->
        case f of
          Free (T.Var a) -> Map.lookup a taus
          _ -> Nothing
      where
        zipM v =
          foldlM (\ m k -> flip (Map.insert k) m <$> v) Map.empty

    skol sigma (T.Forall _a rho) =
      spec sigma rho

    spec sigma rho2 = do
      rho1 <- inst sigma
      mono rho1 rho2

    mono tau tau' =
      void $ unify tau tau'

    lookupPoly x =
      asks (!x)

    insertPoly x sigma =
      local $ Map.insert x sigma

    insertMono x tau =
      local $ Map.insert x (T.Forall Set.empty tau)

    getMono (T.Forall _ rho) = rho

    freezePoly (T.Forall a rho) =
      T.Forall a <$> freeze rho

    getFreeVars m = do
      ms <- universe m
      return $ Set.fromList [a | Pure a <- ms]

    getAllFreeVars ms = do
      ms' <- universeBi ms
      return $ Set.fromList [a | Pure a <- ms']
