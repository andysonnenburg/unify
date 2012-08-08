{-# LANGUAGE PolyKinds #-}
module Main (main) where

import Control.Category ((<<<))
import Control.Monad
import Control.Monad.Error.Wrap
import Control.Monad.Ident
import Control.Monad.Ref.Hashable

import Data.Fix
import qualified Data.HashSet as Set

import Language.HM.DM.Exp
import Language.HM.DM.Type hiding (Var)
import Language.HM.DM.InferType
import Language.HM.Var

main :: IO ()
main =
  print =<<
  runIdentSupplyT
  (runRefSupplyT <<<
   either (fail . show) return <=<
   runWrappedErrorT $
   inferType =<< do
     id <- newVar
     x <- newVar
     return $
       Let id (Fix (AAbs x (Fix Int) (Fix (Var x))))
       (Fix (Var id)))
