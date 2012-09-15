{-# LANGUAGE FlexibleContexts #-}
module Language.HM.Parse
       ( module Exports
       , parse
       ) where

import Control.Monad.Error.Class

import Data.ByteString.Lazy (ByteString)
import Data.Fix
import Data.Flip
import Data.Tagged
import Data.Text

import Language.HM.Exp
import Language.HM.Lex.Internal
import Language.HM.Parse.Internal as Exports hiding (parse)
import qualified Language.HM.Parse.Internal as Internal
import Language.HM.Type

parse :: MonadError ParserError m => ByteString -> m ParsedExp
parse = runParserT Internal.parse
