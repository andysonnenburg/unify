{-# LANGUAGE PolymorphicComponents #-}
module Control.Monad.Trans.Unifier.Either
       ( Either
       , runEither
       , EitherT
       , runEitherT
       ) where

import Data.Functor.Identity

import Prelude hiding (Either)
import qualified Data.Either as Either

type Either e = EitherT e Identity

runEither :: Either e a -> Either.Either e a
runEither = runIdentity . runEitherT

newtype EitherT e m a
  = EitherT { unEitherT :: forall r .
                           (a -> m (Either.Either e r)) ->
                           m (Either.Either e r)
            }

runEitherT :: Monad m => EitherT e m a -> m (Either.Either e a)
runEitherT m = unEitherT m (return . Right)
