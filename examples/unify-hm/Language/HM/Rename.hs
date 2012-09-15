{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , LambdaCase #-}
module Language.HM.Rename
       ( NameError (..)
       , rename
       ) where

import Control.Applicative
import Control.Category
import Control.Monad.Error.Class
import Control.Monad.Name.Class
import Control.Monad.Reader
import Control.Monad.Wrap

import Data.Fix
import Data.Flip
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import Data.Monoid
import Data.Tagged
import Data.Text

import Language.HM.Exp
import Language.HM.Parse
import Language.HM.Type (Mono)
import Language.HM.Var

import Prelude hiding ((.))

type Map = HashMap

type RenamedExp name = Fix (Exp Curry name (Fix (Mono name)))

data NameError
  = NotInScope Text
  | ConflictingDefinitions Text deriving Show

rename :: ( Eq (name Value)
          , Hashable (name Value)
          , MonadError NameError m
          , MonadName name m
          ) =>
          ParsedExp ->
          m (RenamedExp name)
rename = unwrapMonadT . flip runReaderT mempty . loop
  where
    loop = liftM Fix <<< getFix >>> \ case
      Lit i ->
        pure $ Lit i
      Var x ->
        Var <$> lookupName x
      Abs x e ->
        bindName x $ \ x' -> Abs x' <$> loop e
      App e1 e2 ->
        App <$> loop e1 <*> loop e2
      Let x e e' ->
        (bindName x $ \ x' -> Let x' <$> loop e) <*> loop e'

lookupName :: ( MonadError NameError m
              , MonadReader (R name) m
              ) => Flip Tagged Text Value -> m (name Value)
lookupName (Flip (Tagged name)) =
  asks (Map.lookup name) >>= flip whenNothing (throwError $ NotInScope name)

bindName :: ( MonadError NameError m
            , MonadName name m
            , MonadReader (R name) m
            ) => Flip Tagged Text Value -> (name Value -> m a) -> m a
bindName (Flip (Tagged name)) f = do
  r <- ask
  case Map.lookup name r of
    Nothing -> do
      name' <- newVar
      local (Map.insert name name') $ f name'
    Just _ ->
      throwError $ ConflictingDefinitions name

whenNothing :: Monad m => Maybe a -> m a -> m a
whenNothing = flip (flip maybe return)

type R name = Map Text (name Value)
