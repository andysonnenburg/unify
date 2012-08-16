module Main (main) where

import Control.Category ((<<<))
import Control.Monad
import Control.Monad.Error.Wrap
import Control.Monad.Ident
import Control.Monad.Ref.Hashable

import Data.Fix

import Language.HM.DM.Exp
import Language.HM.DM.Type hiding (Var)
import Language.HM.DM.InferType
import Language.HM.Var

import Prelude hiding (id)

main :: IO ()
main =
  runIdentSupplyT
  (liftIO . print <=<
   runRefSupplyT <<<
   either (fail . show) return <=<
   runWrappedErrorT $
   inferType =<< do
     id <- newVar
     x <- newVar
     return $ Fix $
       Let id (Fix (AAbs (x, Fix Int) (Fix (Var x))))
       (Fix (Var id)))
