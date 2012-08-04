{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , ViewPatterns #-}
module Language.HM.DM.TypeCheck
       ( typeCheck
       ) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Monad.Reader hiding (forM_)
import Control.Monad.State hiding (forM_)
import Control.Monad.Unify

import Data.Fix
import Data.Foldable
import Data.Hashable
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set

import Language.HM.DM.Type (Mono, Poly)
import qualified Language.HM.DM.Type as T
import qualified Language.HM.Exp as E

type Exp = E.Exp (Poly Int (Mono Int (Fix (Mono Int))))

typeCheck :: ( Eq a
             , Hashable a
             , Show (ref (Maybe (Term (Mono Int) ref)))
             , MonadUnify (Mono Int) ref m
             ) => Exp a (Fix (Exp a)) -> m (Poly Int (Mono Int (Fix (Mono Int))))
typeCheck =
  flip evalStateT 0 <<<
  flip runReaderT Map.empty <<<
  freezePoly <=<
  poly <=<
  loop
  where
    loop (E.Lit _) =
      return $ wrap $ T.Int
    loop (E.Var x) = do
      sigma <- lookupPoly x
      rho <- inst sigma
      return rho
    loop (E.Abs x t) = do
      tau <- liftM pure newFreeVar
      rho <- insertMono x tau $ loop $ getFix t
      return $ wrap $ T.Fn tau rho
    loop (E.App t u) = do
      tau' <- loop $ getFix t
      tau <- loop $ getFix u
      rho <- liftM pure newFreeVar
      _ <- unify tau' (wrap $ T.Fn tau rho)
      return rho
    loop (E.Let x u t) = do
      sigma <- poly <=< loop <<< getFix $ u
      insertPoly x sigma $ loop $ getFix t
    loop (E.Annot t (fmap unfreeze -> sigma)) = do
      sigma' <- poly <=< loop <<< getFix $ t
      sh sigma' sigma
      inst sigma
      
    poly rho = do
      gamma <- ask
      freeVars <- Set.difference `liftM`
                  getFreeVars rho `ap`
                  getAllFreeVars (getMono <$> gamma)
      as <- foldlM
            (\ as freeVar -> do
                a <- newTypeVar
                _ <- unify (pure freeVar) (wrap $ T.Var a)
                return $ Set.insert a as)
            Set.empty
            freeVars
      return $ T.Forall as rho

    sh sigma sigma' =
      skol sigma sigma'

    skol sigma (T.Forall _a rho) =
      spec sigma rho

    spec sigma rho2 = do
      rho1 <- inst sigma
      mono rho1 rho2

    mono tau tau' = do
      _ <- unify tau tau'
      return ()
      
    inst (T.Forall as rho) = do
      taus <- foldlM (\ taus a -> do
        tau <- liftM pure newFreeVar
        return $ Map.insert a tau taus) Map.empty as
      flip rewrite rho $ \ f ->
        case f of
          Free (T.Var a) -> Map.lookup a taus
          _ -> Nothing

    lookupPoly x =
      asks (!x)

    insertPoly x sigma =
      local $ Map.insert x sigma

    insertMono x tau =
      local $ Map.insert x (T.Forall Set.empty tau)

    getMono (T.Forall _ rho) = rho
      
    freezePoly (T.Forall a rho) =
      liftM (T.Forall a) $ freeze rho
      
    newTypeVar = do
      s <- get
      put $! s + 1
      return s
