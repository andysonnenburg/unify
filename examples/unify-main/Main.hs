{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , FlexibleInstances
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main (main) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Monad.Disj
import Control.Monad.Error.Wrap
import Control.Monad.Ref
import Control.Monad.Unify

import Data.Foldable
import Data.Function
import Data.Traversable

import System.Exit

import Prelude hiding ((==), (&&), (||))

data Dest f
  = Chicago
  | NewYork
  | LosAngeles deriving (Read, Show, Functor, Foldable, Traversable)

instance Unifiable Dest where
  zipMatch = go
    where
      go Chicago Chicago = Just Chicago
      go NewYork NewYork = Just NewYork
      go LosAngeles LosAngeles = Just LosAngeles
      go _ _ = Nothing

main :: IO ()
main =
  either (const exitFailure) (const exitSuccess) <=<
  runWrappedErrorT <<<
  flip runDisjT (<|>) $
  runRefSupplyT
  (uncurry reach =<<
   ((,) `on` wrap . read) <$>
   liftIO getLine <*>
   liftIO getLine)

reach x y =
  x == y || forSome (\ z -> flight x z && reach z y)

flight x y =
  x == wrap Chicago && y == wrap NewYork ||
  x == wrap LosAngeles && y == wrap Chicago ||
  x == wrap Chicago && y == wrap LosAngeles

forSome = (freshTerm >>=)

(&&) = (*>)
infixr 3 &&

(||) = (<|>)
infixr 2 ||

(==) a =
  either (const (throwError ())) (const (return ())) <=<
  runWrappedErrorT <<<
  unify a
infix 4 ==
