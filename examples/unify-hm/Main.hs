module Main (main) where

import Control.Monad

import Data.Fix

import Language.HM.Syntax
import Language.HM.TypeCheck

main :: IO ()
main =
  print <=< typeCheck $
  Abs 0 (Fix (App (Fix (Var 0)) (Fix (Bool True))))