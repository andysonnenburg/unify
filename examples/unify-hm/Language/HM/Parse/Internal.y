{
{-# LANGUAGE DataKinds, NoMonomorphismRestriction #-}
module Language.HM.Parse.Internal
       ( module Exports
       , ParsedExp
       , ParserError (..)
       , parse
       ) where

import Control.Monad.Error.Class

import Data.Fix
import Data.Flip
import Data.Tagged
import Data.Text (Text)

import Language.HM.Exp
import Language.HM.Lex.Internal as Exports
import Language.HM.Token hiding (Let)
import qualified Language.HM.Token as T
import Language.HM.Type (Mono)
import Language.HM.Var

import Prelude hiding (exp, lex)
}

%name exp
%tokentype { LexedToken }

%monad { MonadError ParserError m } { m } { (>>=) } { return }
%lexer { lexer } { EOF }

%token
'\\' { Token Backslash }
'.' { Token Period }
'=' { Token Equals }
LET { Token T.Let }
IN { Token In }
NAME { Token (Name $$) }

%%

exp :: { ParsedExp }
  : var { $1 }
  | abs { $1 }
  | app { $1 }
  | let { $1 }

var :: { ParsedExp }
  : name { Fix $ Var $1 }

abs :: { ParsedExp }
  : '\\' name '.' exp { Fix $ Abs $2 $4 }

app :: { ParsedExp }
  : exp exp { Fix $ App $1 $2 }

let :: { ParsedExp }
  : LET name '=' exp IN exp { Fix $ Let $2 $4 $6 }

name :: { Flip Tagged Text Value }
  : NAME { Flip $ Tagged $1 }

{
type ParsedExp
  = Fix (Exp Curry (Flip Tagged Text) (Fix (Mono (Flip Tagged Text))))

parse :: MonadError ParserError m => ParserT m ParsedExp
parse = exp

happyError :: MonadError ParserError m => m a
happyError = undefined

lexer :: MonadError ParserError m => (LexedToken -> ParserT m a) -> ParserT m a
lexer = (lex >>=)
}
