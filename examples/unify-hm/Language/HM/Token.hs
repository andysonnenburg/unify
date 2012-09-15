module Language.HM.Token
       ( Token (..)
       ) where

import Data.Text (Text)

data Token
  = Backslash
  | Period
  | Equals
  | Let
  | In
  | Name Text deriving Show
