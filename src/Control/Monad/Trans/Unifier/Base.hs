module Control.Monad.Trans.Unifier.Base
       ( WrappedOrd (..)
       ) where

newtype WrappedOrd a = WrapOrd { unwrapOrd :: a } deriving (Eq, Ord)
