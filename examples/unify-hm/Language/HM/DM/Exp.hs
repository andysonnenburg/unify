{-# LANGUAGE
    EmptyDataDecls
  , FlexibleContexts
  , GADTs
  , StandaloneDeriving
  , TypeFamilies #-}
module Language.HM.DM.Exp
       ( Curry, Church
       , Exp (..)
       , Binder
       ) where

import Language.HM.DM.Type
import Language.HM.Var

data Curry
data Church

data Exp style name mono exp where
  Lit :: Int -> Exp style name mono exp
  Var :: name Value -> Exp style name mono exp
  Abs :: Binder style name mono -> exp -> Exp style name mono exp
  AAbs :: (name Value, mono) -> exp -> Exp Curry name mono exp
  TyAbs :: [name Type] -> exp -> Exp Church name mono exp
  App :: exp -> exp -> Exp style name mono exp
  TyApp :: exp -> [mono] -> Exp Church name mono exp
  Let :: Binder style name mono -> exp -> exp -> Exp style name mono exp
  Ann :: exp -> Poly name mono -> Exp Curry name mono exp
deriving instance ( Show (Binder style name mono)
                  , Show (name Value)
                  , Show (name Type)
                  , Show mono
                  , Show exp
                  ) => Show (Exp style name mono exp)

type family Binder style (name :: * -> *) mono
type instance Binder Curry name mono = name Value
type instance Binder Church name mono = (name Value, Poly name mono)
