{-# LANGUAGE
    EmptyDataDecls
  , FlexibleContexts
  , GADTs
  , StandaloneDeriving
  , TypeFamilies #-}
module Language.HM.DM.Exp
       ( I, E
       , Exp (..)
       , Binder
       ) where

import Language.HM.DM.Type
import Language.HM.Var

data I
data E

data Exp k name mono exp where
  Lit :: Int -> Exp k name mono exp
  Var :: name Value -> Exp k name mono exp
  Abs :: Binder k name mono -> exp -> Exp k name mono exp
  AAbs :: (name Value, mono) -> exp -> Exp I name mono exp
  TyAbs :: name Type -> exp -> Exp E name mono exp
  App :: exp -> exp -> Exp k name mono exp
  TyApp :: exp -> mono -> Exp E name mono exp
  Let :: Binder k name mono -> exp -> exp -> Exp k name mono exp
  Ann :: exp -> Poly name mono -> Exp I name mono exp
deriving instance ( Show (Binder k name mono)
                  , Show (name Value)
                  , Show (name Type)
                  , Show mono
                  , Show exp
                  ) => Show (Exp k name mono exp)

type family Binder k (name :: * -> *) mono
type instance Binder I name mono = name Value
type instance Binder E name mono = (name Value, Poly name mono)
