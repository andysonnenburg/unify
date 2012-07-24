{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable #-}
module Main (main) where

import Control.Monad.Error
import Control.Monad.Ref
import Control.Monad.Trans.Unifier

import Data.Foldable
import Data.Traversable

data Type f = Var Int | f :-> f deriving (Show, Functor, Foldable, Traversable)

instance Unifiable Type where
  zipMatch = go
    where
      go (Var a) (Var b) | a == b = Just (Var a)
      go (a :-> b) (a' :-> b') = Just ((a, a') :-> (b, b'))
      go _ _ = Nothing

main :: IO ()
main = runRefSupplyT $
  either (fail . show) return <=< runErrorT $ do
    a <- freshTerm
    b <- freshTerm
    c <- freshTerm
    c <- unify c (wrap $ a :-> b)
    a <- unify a b
    a <- unify a (wrap $ Var 0)
    liftIO . print =<< freeze a
    liftIO . print =<< freeze b
    liftIO . print =<< freeze c
