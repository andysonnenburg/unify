module Main (main) where

import Control.Category ((<<<))
import Control.Monad
import Control.Monad.Error.Wrap
import Control.Monad.Ref.Hashable

import Data.Fix

import Language.HM.Syntax
import Language.HM.TypeCheck

main :: IO ()
main =
  print <=<
  runRefSupplyT <<<
  either (fail . show) return <=<
  runWrappedErrorT <<<
  typeCheck $
  Let 1 (Fix (Abs 0 (Fix (Var 0))))
  (Fix (Let 2 (Fix (App (Fix (Var 1)) (Fix (Bool True)))) (Fix (Var 1))))