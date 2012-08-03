module Language.HM.Exp
       ( Exp (..)
       ) where

data Exp t a f
  = Lit Int
  | Var a
  | Abs a f
  | TAbs a t f
  | App f f
  | Let a f f
  | Annot f t deriving Show

