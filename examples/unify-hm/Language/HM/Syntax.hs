module Language.HM.Syntax
       ( Exp (..)
       ) where

data Exp a f
  = Var a
  | App f f
  | Abs a f
  | Let a f f
  | Bool Bool deriving Show
