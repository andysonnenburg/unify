module Language.HM.TypeCheck
       ( typeCheck
       ) where

import Data.Fix

import Language.HM.Syntax
import Language.HM.Type

typeCheck :: Exp Int (Fix (Exp Int)) -> m (Type Int (Fix (Type Int)))
typeCheck = undefined