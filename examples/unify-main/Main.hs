{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main (main) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Monad.Error.Wrap
import Control.Monad.Ref.Hashable
import Control.Monad.Unify

import Data.Foldable
import Data.Traversable

import Prelude hiding ((==))

data Dest f
  = Chicago
  | NewYork
  | LosAngeles deriving (Show, Functor, Foldable, Traversable)

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
  reach (wrap LosAngeles) (wrap NewYork)
  where
    reach x y =
      x == y <|> do
        z <- freshTerm
        flight x z *> reach z y
    flight x y =
      x == wrap Chicago *> y == wrap NewYork <|>
      x == wrap LosAngeles *> y == wrap Chicago
    (==) a =
      liftM (const ()) <<<
      lift . either throwError return <=<
      liftM (mapLeft (fmap (const ()))) <<<
      runWrappedErrorT <<<
      unify a
    mapLeft f (Left a) = Left (f a)
    mapLeft _ (Right b) = Right b
    
