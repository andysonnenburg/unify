{-# LANGUAGE
    FlexibleContexts
  , StandaloneDeriving
  , UndecidableInstances #-}
module Data.Fix
       ( Fix (..)
       ) where

newtype Fix f = Fix { getFix :: f (Fix f) }

deriving instance Show (f (Fix f)) => Show (Fix f)