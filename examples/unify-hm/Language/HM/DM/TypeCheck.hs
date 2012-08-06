{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , ViewPatterns #-}
module Language.HM.DM.TypeCheck
       ( typeCheck
       ) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Monad.Unify
import Control.Monad.Wrap hiding (mapM)

import Data.Fix
import Data.Foldable
import Data.Hashable
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set

import Language.HM.DM.Type (Mono, Poly)
import qualified Language.HM.DM.Type as T
import qualified Language.HM.Exp as E

import Prelude hiding ((-), mapM)

type Exp = E.Exp (Poly Int (Mono Int (Fix (Mono Int))))

typeCheck :: ( Eq a
             , Hashable a
             , Show (ref (Maybe (Term (Mono Int) ref)))
             , MonadUnify (Mono Int) ref m
             ) =>
             Exp a (Fix (Exp a)) ->
             m (Poly Int (Mono Int (Fix (Mono Int))))
typeCheck =
  unwrapMonadT <<<
  flip evalStateT 0 <<<
  flip runReaderT Map.empty <<<
  freezePoly <=<
  poly
  where
    loop (E.Lit _) =
      return $ wrap T.Int
    loop (E.Var x) = do
      sigma <- lookupPoly x
      inst sigma
    loop (E.Abs x t) = do
      tau <- pure <$> newFreeVar
      rho <- insertMono x tau $ loop $ getFix t
      return $ wrap $ T.Fn tau rho
    loop (E.AAbs x (T.Forall a (unfreeze -> tau)) t)
      | Set.null a = do
        tau' <- pure <$> newFreeVar
        rho <- insertMono x tau' $ loop $ getFix t
        sh (T.Forall Set.empty tau') (T.Forall Set.empty tau)
        return $ wrap $ T.Fn tau rho
    loop (E.App t u) = do
      tau' <- loop $ getFix t
      tau <- loop $ getFix u
      rho <- pure <$> newFreeVar
      _ <- unify tau' (wrap $ T.Fn tau rho)
      return rho
    loop (E.Let x u t) = do
      sigma <- poly $ getFix u
      insertPoly x sigma $ loop $ getFix t
    loop (E.Annot t (fmap unfreeze -> sigma)) = do
      sigma' <- poly $ getFix t
      sh sigma' sigma
      inst sigma

    poly t = do
      rho <- loop t
      gamma <- asks $ fmap getMono
      a <- freezeVars =<< (-) <$> getFreeVars rho <*> getAllFreeVars gamma
      return $ T.Forall a rho
      where
        freezeVars = mapM $ \ freeVar -> do
          a <- newTypeVar
          _ <- unify (pure freeVar) (wrap $ T.Var a)
          return a
        mapM f =
          foldlM (\ a -> fmap (flip Set.insert a) . f) Set.empty
        (-) = Set.difference

    inst (T.Forall as rho) = do
      taus <- foldlM (\ taus a -> do
        tau <- fmap pure newFreeVar
        return $ Map.insert a tau taus) Map.empty as
      flip rewrite rho $ \ f ->
        case f of
          Free (T.Var a) -> Map.lookup a taus
          _ -> Nothing

    sh = skol

    skol sigma (T.Forall _a rho) =
      spec sigma rho

    spec sigma rho2 = do
      rho1 <- inst sigma
      mono rho1 rho2

    mono tau tau' = do
      _ <- unify tau tau'
      return ()

    lookupPoly x =
      asks (!x)

    insertPoly x sigma =
      local $ Map.insert x sigma

    insertMono x tau =
      local $ Map.insert x (T.Forall Set.empty tau)

    getMono (T.Forall _ rho) = rho

    freezePoly (T.Forall a rho) =
      T.Forall a <$> freeze rho

    newTypeVar = do
      s <- get
      put $! s + 1
      return s
