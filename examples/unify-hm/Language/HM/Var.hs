{-# LANGUAGE EmptyDataDecls #-}
module Language.HM.Var
       ( Value
       , Type
       , newVar
       , newTypeVar
       ) where

import Control.Monad.Name.Class

data Value
data Type

newVar :: MonadName name m => m (name Value)
newVar = newName (Proxy :: Proxy Value)

newTypeVar :: MonadName name m => m (name Type)
newTypeVar = newName (Proxy :: Proxy Type)
