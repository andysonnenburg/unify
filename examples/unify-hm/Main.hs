{-# LANGUAGE
    ExistentialQuantification
  , NoMonomorphismRestriction
  , StandaloneDeriving
  , TupleSections #-}
module Main (main) where

import Control.Category ((<<<))
import Control.Monad
import Control.Monad.Error.Wrap
import Control.Monad.Name
import Control.Monad.Reader
import Control.Monad.Ref.Hashable
import Control.Monad.Unify

import Data.ByteString.Lazy as ByteString

import Language.HM.Exp
import Language.HM.InferType
import Language.HM.Parse
import Language.HM.Rename

import Prelude hiding (id)

main :: IO ()
main =
  flip runReaderT Stdin $
  runNameSupplyT
  (runRefSupplyT <<<
   liftIO . print <=<
   either (fail . show) return <=<
   runWrappedErrorT $
   fmap prettyChurch . inferType' =<<
   rename' =<<
   parse' =<<
   liftIO ByteString.getContents)
  where
    inferType' = mapError ((, undefined) . UnificationError) . inferType
    rename' = mapError (mapFst NameError) . rename
    parse' = mapError (mapFst ParserError) . parse

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

data CompilerError
  = forall f ref .
    ( Show (f (Term f ref))
    , Show (ref (Maybe (Term f ref)))
    ) => UnificationError (UnificationError f ref)
  | ParserError ParserError
  | NameError NameError
deriving instance Show CompilerError
