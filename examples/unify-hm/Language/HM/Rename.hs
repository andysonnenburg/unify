{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , LambdaCase
  , TupleSections #-}
module Language.HM.Rename
       ( NameError (..)
       , rename
       ) where

import Control.Applicative
import Control.Category
import Control.Comonad.Cofree
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

type RenamedExp name = Cofree (Exp Curry name (Fix (Mono name))) Loc

data NameError = NotInScope Text deriving Show

rename :: ( Eq (name Value)
          , Hashable (name Value)
          , MonadError (NameError, Loc) m
          , MonadName name m
          ) =>
          ParsedExp ->
          m (RenamedExp name)
rename = unwrapMonadT . flip runReaderT mempty . loop
  where
    loop (a :< f) = (a :<) <$> case f of
      Lit i ->
        pure $ Lit i
      Var x ->
        Var <$> lookupName' x
      Abs x e ->
        bindName x $ \ x' -> Abs x' <$> loop e
      App e1 e2 ->
        App <$> loop e1 <*> loop e2
      Let x e1 e2 -> do
        e1' <- loop e1
        (bindName x $ \ x' -> Let x' e1' <$> loop e2)
      where
        lookupName' = either (throwError . (, a)) return <=< lookupName

lookupName :: MonadReader (R name) m =>
              Flip Tagged Text Value -> m (Either NameError (name Value))
lookupName (Flip (Tagged name)) =
  asks (Map.lookup name) >>= \ case
    Nothing -> return . Left $ NotInScope name
    Just name' -> return $ Right name'

bindName :: ( MonadName name m
            , MonadReader (R name) m
            ) => Flip Tagged Text Value -> (name Value -> m a) -> m a
bindName (Flip (Tagged name)) f = do
  name' <- newVar
  local (Map.insert name name') $ f name'

type R name = Map Text (name Value)
