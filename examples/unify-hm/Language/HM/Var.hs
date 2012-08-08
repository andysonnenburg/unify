{-# LANGUAGE EmptyDataDecls #-}
module Language.HM.Var
       ( Value
       , Type
       , newVar
       , newTypeVar
       ) where

import Control.Monad.Ident.Class

data Value
data Type

newVar :: MonadIdent i m => m (i Value)
newVar = newIdent (index :: Index Value)

newTypeVar :: MonadIdent i m => m (i Type)
newTypeVar = newIdent (index :: Index Type)
