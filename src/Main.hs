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

data Type f = Val | Ptr f | Tuple [f] deriving (Show, Functor, Foldable, Traversable)

instance Unifiable Type where
  zipMatch = go
    where
      go Val Val = Just Val
      go (Ptr a) (Ptr b) = Just (Ptr (a, b))
      go (Tuple xs) (Tuple ys) = Just (Tuple (zip xs ys))
      go _ _ = Nothing

main :: IO ()
main = runRefSupplyT $
  either (fail . show) return =<< runErrorT (do
    a <- freshTerm
    a <- unify a (wrap Val)
    b <- freshTerm
    b <- unify b (wrap $ Ptr a)
    c <- freshTerm
    c <- unify c (wrap $ Ptr b)
    d <- freshTerm
    d <- unify d (wrap $ Tuple [a, b, c])
    liftIO . print =<< freeze d)
