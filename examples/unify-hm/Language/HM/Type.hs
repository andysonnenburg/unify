module Language.HM.Type
       ( Type (..)
       , TypeScheme (..)
       ) where

data Type a f
  = Var a
  | Fn f f
  | Bool deriving Show

data TypeScheme a f
  = Forall [a] (Type a f) deriving Show