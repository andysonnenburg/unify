module Main (main) where

import Control.Category ((<<<))
import Control.Monad
import Control.Monad.Error.Wrap
import Control.Monad.Name
import Control.Monad.Ref.Hashable

import Data.Fix

import Language.HM.DM.Exp
import Language.HM.DM.Type hiding (Var)
import Language.HM.DM.InferType
import Language.HM.Var

import Prelude hiding (id)

main :: IO ()
main =
  runNameSupplyT
  (liftIO . print <=<
   runRefSupplyT <<<
   either (fail . show) return <=<
   runWrappedErrorT $
   fmap prettyChurch . inferType =<< do
     id <- newVar
     x <- newVar
     return $ Fix $
       Let id (Fix (Abs x (Fix (Var x))))
       (Fix (App (Fix (Var id)) (Fix (Lit 0)))))
