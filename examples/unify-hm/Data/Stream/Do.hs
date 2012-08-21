{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Stream.Do
       ( module Exports
       , return
       , (>>=)
       , (>>)
       , fail
       ) where

import qualified Control.Monad as Monad

import Data.Foldable hiding (concatMap)
import qualified Data.List as List
import Data.Stream as Exports

import Prelude hiding (Monad (..), concatMap)

return :: a -> [a]
return a = [a]

class Bind m n where
  (>>=) :: m a -> (a -> n b) -> m b
infixl 1 >>=

instance Bind Stream [] where
  (>>=) = flip concatMap

instance Bind Stream Stream where
  (a :| _) >>= k = k a

instance Bind [] [] where
  (>>=) = (Monad.>>=)

instance Bind [] Stream where
  m >>= k = List.concatMap (toList . k) m

(>>) :: Bind m n => m a -> n b -> m b
m >> n = m >>= \ _ -> n
infixl 1 >>

fail :: String -> m a
fail = error
