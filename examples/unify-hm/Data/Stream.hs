module Data.Stream
       ( Stream (..)
       , head
       , tail
       , take
       , enumFrom
       , iterate
       , concatMap
       , (++)
       ) where

import Data.Foldable hiding (concatMap)

import Prelude hiding ((++),
                       concatMap,
                       enumFrom,
                       foldr,
                       head,
                       iterate,
                       tail,
                       take)

data Stream a = a :| Stream a deriving Show
infixr 5 :|

instance Functor Stream where
  fmap f = go
    where
      go (x :| xs) = f x :| fmap f xs

instance Foldable Stream where
  foldr k _z = go
    where
      go (y :| ys) = y `k` go ys

head :: Stream a -> a
head (x :| _) = x

tail :: Stream a -> Stream a
tail (_ :| xs) = xs

take :: Int -> Stream a -> [a]
take n _ | n <= 0 = []
take n (x :| xs) = x : take (n - 1) xs

enumFrom :: Enum a => a -> Stream a
enumFrom = iterate succ

iterate :: (a -> a) -> a -> Stream a
iterate f = go
  where
    go x = x :| go (f x)

concatMap :: (a -> [b]) -> Stream a -> Stream b
concatMap f = go
  where
    go (x :| xs) = foldr (:|) (go xs) (f x)

(++) :: [a] -> Stream a -> Stream a
(++) [] ys = ys
(++) (x:xs) ys = x :| xs ++ ys
infixr 5 ++
