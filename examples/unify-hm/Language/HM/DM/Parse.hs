module Language.HM.DM.Parse
       ( parse
       ) where

import Data.ByteString.Lazy (ByteString)
import Data.Fix
import Data.Flip
import Data.Tagged
import Data.Text

import Language.HM.DM.Exp
import Language.HM.DM.Type

parse :: ByteString ->
         m (Fix (Exp Curry (Flip Tagged Text) (Fix (Mono (Flip Tagged Text)))))
parse = undefined
