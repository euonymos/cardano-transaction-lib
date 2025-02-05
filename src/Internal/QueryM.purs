-- | CTL query layer monad.
-- | This module defines an Aff interface for backend queries.
module Ctl.Internal.QueryM
  ( QueryM
  , ParQueryM
  , QueryMT(QueryMT)
  , handleAffjaxResponse
  ) where

import Prelude

import Aeson (class DecodeAeson, decodeAeson, parseJsonStringToAeson)
import Affjax (Error, Response) as Affjax
import Affjax.StatusCode as Affjax.StatusCode
import Cardano.Provider.Error
  ( ClientError(ClientHttpError, ClientHttpResponseError, ClientDecodeJsonError)
  , ServiceError(ServiceOtherError)
  )
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Reader.Trans (ReaderT(ReaderT), asks)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Parallel (class Parallel, parallel, sequential)
import Control.Plus (class Plus)
import Ctl.Internal.Helpers (logWithLevel)
import Ctl.Internal.QueryM.Ogmios.Queries (QueryEnv)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right))
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff, ParAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)

type QueryM = QueryMT Aff

type ParQueryM = QueryMT ParAff

newtype QueryMT (m :: Type -> Type) (a :: Type) =
  QueryMT (ReaderT QueryEnv m a)

derive instance Newtype (QueryMT m a) _
derive newtype instance Functor m => Functor (QueryMT m)
derive newtype instance Apply m => Apply (QueryMT m)
derive newtype instance Applicative m => Applicative (QueryMT m)
derive newtype instance Bind m => Bind (QueryMT m)
derive newtype instance Alt m => Alt (QueryMT m)
derive newtype instance Plus m => Plus (QueryMT m)
derive newtype instance Alternative m => Alternative (QueryMT m)
derive newtype instance Monad (QueryMT Aff)
derive newtype instance MonadEffect (QueryMT Aff)
derive newtype instance MonadAff (QueryMT Aff)
derive newtype instance
  ( Semigroup a
  , Apply m
  ) =>
  Semigroup (QueryMT m a)

derive newtype instance
  ( Monoid a
  , Applicative m
  ) =>
  Monoid (QueryMT m a)

derive newtype instance MonadThrow Error (QueryMT Aff)
derive newtype instance MonadError Error (QueryMT Aff)
derive newtype instance MonadRec (QueryMT Aff)
derive newtype instance MonadAsk QueryEnv (QueryMT Aff)
derive newtype instance MonadReader QueryEnv (QueryMT Aff)

instance MonadLogger (QueryMT Aff) where
  log msg = do
    config <- asks $ _.config
    let
      logFunction =
        config # _.customLogger >>> fromMaybe logWithLevel
    liftAff $ logFunction config.logLevel msg

-- Newtype deriving complains about overlapping instances, so we wrap and
-- unwrap manually
instance Parallel (QueryMT ParAff) (QueryMT Aff) where
  parallel :: QueryMT Aff ~> QueryMT ParAff
  parallel = wrap <<< parallel <<< unwrap
  sequential :: QueryMT ParAff ~> QueryMT Aff
  sequential = wrap <<< sequential <<< unwrap

--------------------------------------------------------------------------------
-- Affjax
--------------------------------------------------------------------------------

-- Checks response status code and returns `ClientError` in case of failure,
-- otherwise attempts to decode the result.
--
-- This function solves the problem described there:
-- https://github.com/eviefp/purescript-affjax-errors
handleAffjaxResponse
  :: forall (result :: Type)
   . DecodeAeson result
  => Either Affjax.Error (Affjax.Response String)
  -> Either ClientError result
handleAffjaxResponse (Left affjaxError) =
  Left (ClientHttpError affjaxError)
handleAffjaxResponse
  (Right { status: Affjax.StatusCode.StatusCode statusCode, body })
  | statusCode < 200 || statusCode > 299 =
      Left $ ClientHttpResponseError (wrap statusCode) $ ServiceOtherError body
  | otherwise =
      body # lmap (ClientDecodeJsonError body)
        <<< (decodeAeson <=< parseJsonStringToAeson)

