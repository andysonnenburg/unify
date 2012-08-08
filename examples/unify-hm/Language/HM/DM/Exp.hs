{-# LANGUAGE
    FlexibleContexts
  , StandaloneDeriving #-}
module Language.HM.DM.Exp
       ( Exp (..)
       ) where

import Language.HM.DM.Type
import Language.HM.Var

import Data.Fix

data Exp a f
  = Lit Int
  | Var (a Value)
  | Abs (a Value) f
  | AAbs (a Value) (Fix (Mono a)) f
  | App f f
  | Let (a Value) f f
  | Annot f (Poly a (Fix (Mono a)))
deriving instance (Show (a Value), Show (a Type), Show f) => Show (Exp a f)
