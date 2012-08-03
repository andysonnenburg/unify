{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts #-}
module Language.HM.TypeCheck
       ( typeCheck
       ) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Monad.Reader hiding (forM_)
import Control.Monad.State hiding (forM_)
import Control.Monad.Unify

import Data.Fix
import Data.Foldable
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as Map

import Language.HM.Syntax as E
import Language.HM.Type as T

typeCheck :: MonadUnify (Type Int) ref m =>
             Exp Int (Fix (Exp Int)) -> m (Type Int (Fix (Type Int)))
typeCheck =
  flip evalStateT 0 <<<
  flip runReaderT Map.empty <<<
  freeze <=<
  generalize <=<
  loop
  where
    loop (E.Var x) =
      asks (!x)
    loop (E.App e1 e2) = do
      f <- loop . getFix $ e1
      a <- loop . getFix $ e2
      b <- liftM pure newFreeVar
      _ <- unify f . wrap $ T.Fn a b
      return b
    loop (E.Abs x e) = do
      a <- liftM pure newFreeVar
      b <- local (Map.insert x a) . loop . getFix $ e
      return . wrap $ Fn a b
    loop (E.Let x e e') = do
      a <- loop . getFix $ e
      local (Map.insert x a) . loop . getFix $ e'
    loop (E.Bool _) =
      return . wrap $ T.Bool

generalize :: ( MonadUnify (Type Int) ref m
              , MonadState Int m
              ) => Term (Type Int) ref -> m (Term (Type Int) ref)
generalize t = do
  freeVars <- getFreeVars t
  forM_ freeVars $ \ freeVar -> do
    typeVar <- newTypeVar
    _ <- unify (pure freeVar) (wrap $ T.Var typeVar)
    return ()
  return t

newTypeVar :: MonadState Int m => m Int
newTypeVar = do
  s <- get
  put $! s + 1
  return s
