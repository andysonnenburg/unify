{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , GADTs
  , LambdaCase
  , RecordWildCards
  , StandaloneDeriving
  , TypeFamilies
  , UndecidableInstances #-}
module Language.HM.Exp
       ( Style (..)
       , Exp (..)
       , Binder
       , prettyChurch
       ) where

import Control.Applicative
import Control.Category
import Control.Monad.Reader
import Control.Monad.State

import Data.Fix
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Monoid hiding ((<>))
import Data.Stream

import Language.HM.Type (Mono, Poly)
import qualified Language.HM.Type as T
import Language.HM.Var

import Text.PrettyPrint.Free

import Prelude hiding ((.), (++), concatMap, enumFrom, head, id, tail)

type Map = HashMap
type Set = HashSet

data Style = Church | Curry

data Exp style name mono exp where
  Lit :: Int -> Exp style name mono exp
  Var :: name Value -> Exp style name mono exp
  Abs :: Binder style name mono -> exp -> Exp style name mono exp
  AAbs :: (name Value, mono) -> exp -> Exp Curry name mono exp
  TyAbs :: Set (name Type) -> exp -> Exp Church name mono exp
  App :: exp -> exp -> Exp style name mono exp
  TyApp :: exp -> Map (name Type) mono -> Exp Church name mono exp
  Let :: Binder style name mono -> exp -> exp -> Exp style name mono exp
  Ann :: exp -> Poly name mono -> Exp Curry name mono exp
deriving instance ( Show (Binder style name mono)
                  , Show (name Value)
                  , Show (name Type)
                  , Show mono
                  , Show exp
                  ) => Show (Exp style name mono exp)

type family Binder (style :: Style) (name :: VarKind -> *) mono
type instance Binder Curry name mono = name Value
type instance Binder Church name mono = (name Value, Poly name mono)

prettyChurch :: ( Eq (name Value)
                , Eq (name Type)
                , Hashable (name Value)
                , Hashable (name Type)
                , Show (name Value)
                , Show (name Type)
                ) => Fix (Exp Church name (Fix (Mono name))) -> Doc e
prettyChurch = flip runReader (0, Nothing) . flip evalStateT initS . loop
  where
    asChurch :: Fix (Exp Church name mono) -> Fix (Exp Church name mono)
    asChurch = id
    loop = asChurch >>> getFix >>> \ case
      Lit i ->
        return $ pretty i
      Var x ->
        prettyValueName x
      Abs (x, sigma) t -> localFixity (Fixity 0 InfixL) $ do
        x' <- prettyValueName x
        sigma' <- prettySigma sigma
        t' <- loop t
        return $ smallLambda <+> x' <> colon <+> sigma' <+> dot <+> t'
      TyAbs a t
        | Set.null a ->
          localFixity (Fixity 0 InfixL) $ loop t
        | otherwise -> localFixity (Fixity 0 InfixL) $ do
          a' <- fmap hsep . mapM prettyTypeName $ Set.toList a
          t' <- loop t
          return $ capitalLambda <+> a' <+> dot <+> t'
      App t u -> localFixity (Fixity 10 InfixN) $ do
        t' <- loop t
        u' <- loop u
        return $ t' <+> u'
      TyApp e t
        | Map.null t ->
          loop e
        | otherwise ->
          localFixity (Fixity 10 InfixN) $
          (<+>) <$> loop e <*>
          (fmap list . forM (Map.toList t) $ \ (k, v) -> do
            k' <- prettyTypeName k
            v' <- prettyMono v
            return $ k' <+> rightwardsArrowFromBar <+> v')
      Let (x, sigma) u t -> localFixity (Fixity 0 InfixL) $ do
        x' <- prettyValueName x
        sigma' <- prettySigma sigma
        u' <- loop u
        t' <- loop t
        return $
          text "let" <+> x' <> colon <+> sigma' <+> equals <+> u' <+>
          text "in" <+> t'
    prettyValueName x = do
      S {..} <- get
      let name = ValueName x
      whenNothing (Map.lookup name names) $ do
        let x' = char 'x' <> pretty valueNameCount
        modify $ \ s ->
          s { valueNameCount = valueNameCount + 1
            , names = Map.insert name x' names
            }
        return x'
    prettyTypeName a = do
      S {..} <- get
      let name = TypeName a
      whenNothing (Map.lookup name names) $ do
        let a' = head typeNames
        modify $ \ s ->
          s { typeNames = tail typeNames
            , names = Map.insert name a' names
            }
        return a'
    whenNothing = flip (flip maybe return)
    prettySigma (T.Forall a rho)
      | Set.null a =
        prettyMono rho
      | otherwise = do
        a' <- fmap hsep . mapM prettyTypeName $ Set.toList a
        rho' <- prettyMono rho
        return $ forAll <+> a' <+> dot <+> rho'
    prettyMono = local (const (0, Nothing)) . loop
      where
        loop = getFix >>> \ case
          T.Int ->
            return $ text "Int"
          T.Fn a b -> localFixity (Fixity 9 InfixR) $ do
            a' <- localSide L $ loop a
            b' <- localSide R $ loop b
            return $ a' <+> rightwardsArrow <+> b'
          T.Var a ->
            prettyTypeName a
    localFixity (Fixity prec' dir) m = do
      (prec, side) <- ask
      let comparePrec =
            case (dir, side) of
              (InfixL, Just L) -> (<)
              (InfixR, Just R) -> (<)
              (_, Nothing) -> (<)
              _ -> (<=)
      localPrec prec' $
        if comparePrec prec' prec
        then enclose lparen rparen <$> m
        else m
    localPrec prec' = local $ \ (_prec, side) -> (prec', side)
    localSide side' = local $ \ (prec, _side) -> (prec, Just side')
    initS =
      S { valueNameCount = 0
        , typeNames =
           fmap char ['a' .. 'z'] ++ 
           enumFrom (0 :: Integer) `bind` \ i ->
             (<> pretty i) . char <$> ['a' .. 'z']
        , names = mempty
        }
      where
        bind = flip concatMap

data Fixity = Fixity Int FixityDirection

data FixityDirection = InfixL | InfixR | InfixN deriving Eq

data Side = L | R

capitalLambda :: Doc e
capitalLambda = char '\x039b'

forAll :: Doc e
forAll = char '\x2200'

rightwardsArrow :: Doc e
rightwardsArrow = char '\x2192'

rightwardsArrowFromBar :: Doc e
rightwardsArrowFromBar = char '\x21a6'

smallLambda :: Doc e
smallLambda = char '\x03bb'

data S name e
  = S { valueNameCount :: Int
      , typeNames :: Stream (Doc e)
      , names :: Map (Name name) (Doc e)
      }

data Name name
  = ValueName (name Value)
  | TypeName (name Type)

deriving instance (Eq (name Value), Eq (name Type)) => Eq (Name name)

instance ( Hashable (name Value)
         , Hashable (name Type)
         ) => Hashable (Name name) where
  hash (ValueName x) = 0 `hashWithSalt` x
  hash (TypeName a) = 1 `hashWithSalt` a
