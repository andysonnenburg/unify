module Control.Monad.Unify.Base
       ( WrappedOrd (..)
       ) where

newtype WrappedOrd a = WrapOrd { unwrapOrd :: a } deriving (Eq, Ord)
