{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable #-}
module Main (main) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Monad.Error.Wrap
import Control.Monad.Ref.Hashable
import Control.Monad.Unify

import Data.Foldable
import Data.Function
import Data.Traversable

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
  print <=<
  runWrappedErrorT <<<
  runRefSupplyT $
  uncurry reach =<<
  ((,) `on` wrap . read) <$>
  liftIO getLine <*>
  liftIO getLine
  where
    reach x y =
      x == y || forSome (\ z -> flight x z && reach z y)
    flight x y = 
      x == wrap Chicago && y == wrap NewYork ||
      x == wrap LosAngeles && y == wrap Chicago
    forSome = (freshTerm >>=)
    (&&) = (*>)
    infixr 3 &&
    (||) = (<|>)
    infixr 2 ||
    (==) a =
      liftM (const ()) <<<
      lift . either (const (throwError ())) return <=<
      runWrappedErrorT <<<
      unify a
    infix 4 ==
    
