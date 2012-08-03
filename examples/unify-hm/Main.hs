module Main (main) where

import Control.Category ((<<<))
import Control.Monad
import Control.Monad.Error.Wrap
import Control.Monad.Ref.Hashable

import Data.Fix

import Language.HM.Exp
import qualified Language.HM.DM.TypeCheck as DM

main :: IO ()
main =
  print <=<
  runRefSupplyT <<<
  either (fail . show) return <=<
  runWrappedErrorT <<<
  DM.typeCheck $
  Abs 0 (Fix (Var 0))
