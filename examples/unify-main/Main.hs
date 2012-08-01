{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable #-}
module Main (main) where

import Control.Monad.Error.Wrap
import Control.Monad.Ref.Hashable
import Control.Monad.Unify

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
  either (fail . show) return <=< runWrappedErrorT $ do
    a <- freshTerm
    b <- freshTerm
    c <- freshTerm
    c <- unify c (wrap $ a :-> b)
    liftIO . print =<< freeVars c
    a <- unify a b
    liftIO . print =<< freeVars c
    a <- unify a (wrap $ Var 0)
    liftIO . print =<< freeze a
    liftIO . print =<< freeze b
    liftIO . print =<< freeze c
    d <- freshTerm
    d <- unify d (wrap $ d :-> d)
    liftIO . print =<< freeVars d
    liftIO . print =<< freeze d
