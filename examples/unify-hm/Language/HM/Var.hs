{-# LANGUAGE DataKinds #-}
module Language.HM.Var
       ( Kind (..)
       , newVar
       , newTypeVar
       ) where

import Control.Monad.Ident.Class

data Kind = Value | Type

newVar :: MonadIdent i m => m (i Value)
newVar = newIdent (index :: Index Value)

newTypeVar :: MonadIdent i m => m (i Type)
newTypeVar = newIdent (index :: Index Type)
