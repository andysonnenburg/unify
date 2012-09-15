{
{-# LANGUAGE
    CPP
  , FlexibleContexts
  , NoMonomorphismRestriction #-}
{-# OPTIONS_GHC
    -fno-warn-lazy-unlifted-bindings
    -fno-warn-missing-signatures
    -fno-warn-unused-binds
    -fno-warn-unused-imports
    -fno-warn-unused-matches #-}
module Language.HM.Lex.Internal
       ( ParserT
       , ParserError (..)
       , runParserT
       , Lexed (..)
       , LexedToken
       , lex
       ) where

import Control.Monad.Error.Class
import Control.Monad.State

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Text.Encoding.Error
import Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding
import Data.Word

import Language.HM.Token

import Prelude hiding (lex)
}

$alpha = [a-zA-Z_]
$numeric = [0-9]

@name = $alpha [$alpha $numeric]*

:-

$white+ ;

<0> {
  \\ { lexBackslash }
  "." { lexPeriod }
  "=" { lexEquals }
  "let" { lexLet }
  "in" { lexIn }
  @name { lexName }
}

{
type Action m = ByteString -> Int -> ParserT m LexedToken

type AlexInput = ByteString

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte = ByteString.uncons

type ParserT = StateT S

data ParserError
  = ParseError Token
  | LexicalError
  | UnicodeException UnicodeException deriving Show

runParserT :: Monad m => ParserT m a -> ByteString -> m a
runParserT = evalStateT

data Lexed t
  = Token !t
  | EOF deriving Show

type LexedToken = Lexed Token

type S = ByteString

lex :: MonadError ParserError m => ParserT m LexedToken
lex = do
  s <- get
  case alexScan s 0 of
    AlexEOF ->
      return $ EOF
    AlexError _ ->
      throwError LexicalError
    AlexSkip s' _ -> do
      put s'
      lex
    AlexToken s' n m -> do
      put s'
      m s n

lexBackslash :: Monad m => Action m
lexBackslash _ _ = return $ Token Backslash

lexPeriod :: Monad m => Action m
lexPeriod _ _ = return $ Token Period

lexEquals :: Monad m => Action m
lexEquals _ _ = return $ Token Equals

lexLet :: Monad m => Action m
lexLet _ _ = return $ Token Let

lexIn :: Monad m => Action m
lexIn _ _ = return $ Token In

lexName :: MonadError ParserError m => Action m
lexName xs n =
  either
  (throwError . UnicodeException)
  (return . Token . Name) .
  fmap Text.toStrict . decodeUtf8' . take' n $ xs
  where
    take' = ByteString.take . fromIntegral

alex_scan_tkn :: MonadError ParserError m =>
                 a ->
                 AlexInput -> Int# ->
                 AlexInput -> Int# ->
                 AlexLastAcc (Action m) ->
                 (AlexLastAcc (Action m), AlexInput)

alex_accept :: MonadError ParserError m => Array Int [AlexAcc (Action m) user]

#define alexRightContext {-
#define iUnbox -} --
}
