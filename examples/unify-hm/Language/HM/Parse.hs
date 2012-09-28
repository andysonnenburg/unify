{-# LANGUAGE FlexibleContexts #-}
module Language.HM.Parse
       ( module Exports
       , parse
       ) where

import Control.Monad.Error.Class
import Control.Monad.Reader.Class

import Data.ByteString.Lazy (ByteString)

import Language.HM.Lex.Internal
import Language.HM.Parse.Internal as Exports hiding (parse)
import qualified Language.HM.Parse.Internal as Internal

parse :: ( MonadError (ParserError, Loc) m
         , MonadReader Src m
         ) => ByteString -> m ParsedExp
parse = runParserT Internal.parse
