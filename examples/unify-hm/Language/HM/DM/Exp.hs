{-# LANGUAGE
    EmptyDataDecls
  , FlexibleContexts
  , GADTs
  , RecordWildCards
  , StandaloneDeriving
  , TypeFamilies
  , UndecidableInstances #-}
module Language.HM.DM.Exp
       ( Curry, Church
       , Exp (..)
       , Binder
       , prettyChurch
       ) where

import Control.Applicative
import Control.Monad.State

import Data.Fix
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import Data.Monoid hiding ((<>))

import Language.HM.DM.Type (Mono, Poly)
import qualified Language.HM.DM.Type as T
import Language.HM.Var

import Text.PrettyPrint.Free

type Map = HashMap

data Curry
data Church

data Exp style name mono exp where
  Lit :: Int -> Exp style name mono exp
  Var :: name Value -> Exp style name mono exp
  Abs :: Binder style name mono -> exp -> Exp style name mono exp
  AAbs :: (name Value, mono) -> exp -> Exp Curry name mono exp
  TyAbs :: [name Type] -> exp -> Exp Church name mono exp
  App :: exp -> exp -> Exp style name mono exp
  TyApp :: exp -> [mono] -> Exp Church name mono exp
  Let :: Binder style name mono -> exp -> exp -> Exp style name mono exp
  Ann :: exp -> Poly name mono -> Exp Curry name mono exp
deriving instance ( Show (Binder style name mono)
                  , Show (name Value)
                  , Show (name Type)
                  , Show mono
                  , Show exp
                  ) => Show (Exp style name mono exp)

type family Binder style (name :: * -> *) mono
type instance Binder Curry name mono = name Value
type instance Binder Church name mono = (name Value, Poly name mono)

prettyChurch :: ( Eq (name Value)
                , Eq (name Type)
                , Hashable (name Value)
                , Hashable (name Type)
                , Show (name Value)
                , Show (name Type)
                ) => Fix (Exp Church name (Fix (Mono name))) -> Doc e
prettyChurch = flip evalState initS . go . getFix
  where
    go (Lit i) =
      return $ pretty i
    go (Var x) =
      prettyValueName x
    go (Abs (x, sigma) t) = do
      x' <- prettyValueName x
      sigma' <- prettySigma sigma
      t' <- go $ getFix t
      return $ char '\x03bb' <+> x' <> colon <+> sigma' <+> dot <+> t'
    go (TyAbs a t) = do
      a' <- hsep <$> mapM prettyTypeName a
      t' <- go $ getFix t
      return $ char '\x039b' <+> a' <+> dot <+> t'
    go (App t u) = do
      t' <- go $ getFix t
      u' <- go $ getFix u
      return $ t' <+> u'
    go (TyApp e t) = do
      e' <- go $ getFix e
      t' <- mapM prettyMono t
      return $ hsep $ e':t'
    go (Let (x, sigma) u t) = do
      x' <- prettyValueName x
      sigma' <- prettySigma sigma
      u' <- go $ getFix u
      t' <- go $ getFix t
      return $
        text "let" <+> x' <> colon <+> sigma' <+> equals <+> u' <+>
        text "in" <+> t'
    prettyValueName =
      prettyName ValueName (char 'x')
    prettyTypeName =
      prettyName TypeName (char 'a')
    prettyName f prefix x = do
      S {..} <- get
      let name = f x
      case Map.lookup name names of
        Nothing -> do
          let x' = prefix <> pretty nameCount
          modify $ \ s ->
            s { nameCount = nameCount + 1
              , names = Map.insert name x' names
              }
          return x'
        Just x' ->
          return x'
    prettySigma (T.Forall a rho) = do
      a' <- hsep <$> mapM prettyTypeName a
      rho' <- prettyMono rho
      return $ char '\x2200' <+> a' <+> dot <+> rho'
    prettyMono rho =
      case getFix rho of
        T.Int ->
          return $ text "int"
        T.Fn a b -> do
          a' <- prettyMono a
          b' <- prettyMono b
          return $ a' <+> char '\x2192' <+> b'
        T.Var a ->
          prettyTypeName a
    initS =
      S { nameCount = 0
        , names = mempty
        }

data S name e
  = S { nameCount :: Int
      , names :: Map (Name name) (Doc e)
      }

data Name name
  = ValueName (name Value)
  | TypeName (name Type)

deriving instance (Eq (name Value), Eq (name Type)) => Eq (Name name)

instance (Hashable (name Value), Hashable (name Type)) => Hashable (Name name) where
  hash (ValueName x) = 0 `hashWithSalt` x
  hash (TypeName a) = 1 `hashWithSalt` a
