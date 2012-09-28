{
{-# LANGUAGE DataKinds, NoMonomorphismRestriction, ViewPatterns #-}
module Language.HM.Parse.Internal
       ( module Exports
       , ParsedExp
       , ParserError (..)
       , parse
       ) where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.Error.Class
import Control.Monad.Reader.Class

import Data.Fix
import Data.Flip
import Data.List.NonEmpty (NonEmpty (..))
import Data.Tagged
import Data.Text (Text)

import Language.HM.Exp
import Language.HM.Lex.Internal as Exports
import Language.HM.Loc.Unqualified as Exports
import Language.HM.Token hiding (Let)
import qualified Language.HM.Token as T
import Language.HM.Type (Mono)
import Language.HM.Var

import Prelude hiding (exp, lex)
}

%name exp
%tokentype { (LexedToken, Loc) }

%monad { MonadError (ParserError, Loc) m } { m } { (>>=) } { return }
%lexer { lexer } { (fst -> EOF) }
%error { happyError }

%token
'\\' { (fst -> Token Backslash) }
'.' { (fst -> Token Period) }
'=' { (fst -> Token Equals) }
LET { (fst -> Token T.Let) }
IN { (fst -> Token In) }
NAME { (fst -> Token (Name _)) }

%%

exp :: { ParsedExp }
  : var { $1 }
  | abs { $1 }
  | app { $1 }
  | let { $1 }

var :: { ParsedExp }
  : name { snd $1 :< Var (fst $1) }

abs :: { ParsedExp }
  : '\\' name '.' exp {
      getLoc (concatL $ L $1:|L $2:L $3:L $4:[]) :<
      Abs (fst $2) $4
    }

app :: { ParsedExp }
  : exp exp {
      getLoc (concatL $ L $1:|L $2:[]) :<
      App $1 $2
    }

let :: { ParsedExp }
  : LET name '=' exp IN exp {
      getLoc (concatL $ L $1:|L $2:L $3:L $4:L $5:L $6:[]) :<
      Let (fst $2) $4 $6
    }

name :: { (Flip Tagged Text Value, Loc) }
  : NAME { mapFst (Flip . Tagged . name) $1 }

{
type ParsedExp
  = Cofree (Exp Curry (Flip Tagged Text) (Fix (Mono (Flip Tagged Text)))) Loc

parse :: ( MonadError (ParserError, Loc) m
         , MonadReader Src m
         ) => ParserT m ParsedExp
parse = exp

happyError :: MonadError (ParserError, Loc) m => (LexedToken, Loc) -> m a
happyError = throwError . mapFst ParseError

name :: LexedToken -> Text
name (Token (Name x)) = x

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

lexer :: ( MonadError (ParserError, Loc) m
         , MonadReader Src m
         ) => ((LexedToken, Loc) -> ParserT m a) -> ParserT m a
lexer = (lex >>=)

class HasLoc a where
  getLoc :: a -> Loc

instance HasLoc Loc where
  getLoc = id

instance HasLoc (a, Loc) where
  getLoc = snd

instance HasLoc (Cofree f Loc) where
  getLoc (a :< _) = a

data L = forall a . HasLoc a => L { unL :: a }

concatL :: NonEmpty L -> L
concatL (x :| xs) = L $ foldr appendLoc (getLoc x) (map getLoc xs)
  where
    appendLoc (Loc src start end) (Loc src' start' end')
      | src == src' = Loc src (min start start') (max end end')
      | otherwise = Loc src start start

instance HasLoc L where
  getLoc (L a) = getLoc a
}
