{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts #-}
module Language.HM.DM.TypeCheck
       ( typeCheck
       ) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Monad.Reader hiding (forM_)
import Control.Monad.Unify

import Data.Fix
import Data.Foldable
import Data.Hashable
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set

import Language.HM.DM.Type (Mono)
import qualified Language.HM.DM.Type as T
import qualified Language.HM.Exp as E

type Exp = E.Exp (Mono Int (Fix (Mono Int)))

typeCheck :: ( Eq a
             , Hashable a
             , MonadUnify (Mono Int) ref m
             ) => Exp a (Fix (Exp a)) -> m (Mono Int (Fix (Mono Int)))
typeCheck =
  flip runReaderT Map.empty <<<
  freeze <=<
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
      rho <- insertTerm x tau $ loop $ getFix t
      return $ wrap $ T.Fn tau rho
    lookupPoly x =
      asks (!x)
    inst (T.Forall a rho) =
      undefined
    insertTerm x tau =
      local $ Map.insert x (T.Forall Set.empty tau)
