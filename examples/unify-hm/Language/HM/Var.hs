{-# LANGUAGE DataKinds #-}
module Language.HM.Var
       ( VarKind (..)
       , newVar
       , newTypeVar
       ) where

import Control.Monad.Name

data VarKind = Value | Type

newVar :: MonadName name m => m (name Value)
newVar = newName (Proxy :: Proxy Value)

newTypeVar :: MonadName name m => m (name Type)
newTypeVar = newName (Proxy :: Proxy Type)
