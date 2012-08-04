module Main (main) where

import Control.Category ((<<<))
import Control.Monad
import Control.Monad.Error.Wrap
import Control.Monad.Ref.Hashable

import Data.Fix
import qualified Data.HashSet as Set

import Language.HM.Exp
import qualified Language.HM.DM.Type as DM
import qualified Language.HM.DM.TypeCheck as DM

main :: IO ()
main =
  print <=<
  runRefSupplyT <<<
  either (fail . show) return <=<
  runWrappedErrorT <<<
  DM.typeCheck $
  Let "id" (Fix (Abs "x" (Fix (Var "x"))))
  (Fix (Annot (Fix (Var "id"))
        (DM.Forall Set.empty
         (DM.Fn (Fix DM.Int) (Fix DM.Int)))))
