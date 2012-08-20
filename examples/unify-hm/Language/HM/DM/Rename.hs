module Language.HM.DM.Rename
       ( rename
       ) where

import Control.Monad.Name.Class

import Data.Fix
import Data.Flip
import Data.Hashable
import Data.Tagged

import Language.HM.DM.Exp
import Language.HM.DM.Type

rename :: ( Eq name
          , Hashable name
          , MonadName name' m
          ) =>
          Fix (Exp Curry (Flip Tagged name) (Fix (Mono (Flip Tagged name)))) ->
          m (Fix (Exp Curry name' (Fix (Mono name'))))
rename = undefined
