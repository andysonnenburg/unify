{-# LANGUAGE PolyKinds #-}
module Data.Flip
       ( Flip (..)
       ) where

newtype Flip f a b = Flip { getFlip :: f b a }
