module Main (main) where

import Control.Category ((<<<))
import Control.Monad
import Control.Monad.Error.Wrap
import Control.Monad.Name
import Control.Monad.Ref.Hashable

import Data.ByteString.Lazy as ByteString

import Language.HM.DM.Exp
import Language.HM.DM.InferType
import Language.HM.DM.Parse
import Language.HM.DM.Rename

import Prelude hiding (id)

main :: IO ()
main =
  runNameSupplyT
  (liftIO . print <=<
   runRefSupplyT <<<
   either (fail . show) return <=<
   runWrappedErrorT $
   fmap prettyChurch . inferType =<<
   rename =<<
   parse =<<
   liftIO ByteString.getContents)
