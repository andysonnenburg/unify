{
{-# LANGUAGE
    CPP
  , FlexibleContexts
  , LambdaCase
  , NamedFieldPuns
  , NoMonomorphismRestriction
  , RecordWildCards
  , TupleSections #-}
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

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Text.Encoding.Error
import Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding
import Data.Word

import Language.HM.Loc.Unqualified
import qualified Language.HM.Loc as Loc
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
type Action m = ByteString -> Int -> m (Either ParserError LexedToken)

type AlexInput = S

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte S { charPos = start, ..} =
  flip fmap (ByteString.uncons byteString) $ \ (x, xs) ->
  let end = seekCharPos start $ w2c x
  in (x, S { charPos = end, byteString = xs })
  where
    seekCharPos (r, c) =
      \ case
        '\t' -> (r, ((c + 7) `div` 8) * 8 + 1)
        '\n' -> (r + 1, 1)
        _ -> (r, c + 1)

type ParserT = StateT S

data ParserError
  = ParseError LexedToken
  | LexicalError
  | UnicodeException UnicodeException deriving Show

data S = S { byteString :: ByteString, charPos :: CharPos }

runParserT :: Monad m => ParserT m a -> ByteString -> m a
runParserT m byteString = evalStateT m S { byteString, charPos = (0, 0) }

data Lexed t
  = Token !t
  | EOF deriving Show

type LexedToken = Lexed Token

lex :: ( MonadError (ParserError, Loc) m
       , MonadReader Src m
       ) => ParserT m (LexedToken, Loc)
lex = do
  src <- ask
  s@S { charPos = start, .. } <- get
  case alexScan s 0 of
    AlexEOF ->
      return (EOF, Loc { Loc.src, Loc.start, Loc.end = start })
    AlexError S { charPos = end } ->
      throwError (LexicalError, Loc { Loc.src, Loc.start, Loc.end })
    AlexSkip s' _ -> do
      put s'
      lex
    AlexToken s'@S { charPos = end } n m -> do
      put s'
      let loc = Loc { Loc.src, Loc.start, Loc.end }
      m byteString n >>= either (throwError . (, loc)) (return . (, loc))

lexBackslash :: Monad m => Action m
lexBackslash _ _ = return' $ Token Backslash

lexPeriod :: Monad m => Action m
lexPeriod _ _ = return' $ Token Period

lexEquals :: Monad m => Action m
lexEquals _ _ = return' $ Token Equals

lexLet :: Monad m => Action m
lexLet _ _ = return' $ Token Let

lexIn :: Monad m => Action m
lexIn _ _ = return' $ Token In

lexName :: Monad m => Action m
lexName xs n =
  either
  (throwError' . UnicodeException)
  (return' . Token . Name) .
  fmap Text.toStrict . decodeUtf8' . take' n $ xs
  where
    take' = ByteString.take . fromIntegral

return' :: Monad m => a -> m (Either e a)
return' = return . Right

throwError' :: Monad m => e -> m (Either e a)
throwError' = return . Left

alex_scan_tkn :: MonadError (ParserError, Loc) m =>
                 a ->
                 AlexInput -> Int# ->
                 AlexInput -> Int# ->
                 AlexLastAcc (Action m) ->
                 (AlexLastAcc (Action m), AlexInput)

alex_accept :: Monad m => Array Int [AlexAcc (Action m) user]

#define alexRightContext {-
#define iUnbox -} --
}
