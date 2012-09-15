{-# LANGUAGE FlexibleContexts #-}
module Language.HM.Parse
       ( module Exports
       , parse
       ) where

import Control.Monad.Error.Class

import Data.ByteString.Lazy (ByteString)

import Language.HM.Lex.Internal
import Language.HM.Parse.Internal as Exports hiding (parse)
import qualified Language.HM.Parse.Internal as Internal

parse :: MonadError ParserError m => ByteString -> m ParsedExp
parse = runParserT Internal.parse
