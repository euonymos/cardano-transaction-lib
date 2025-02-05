-- | A module to get "eraSummaries" via an Ogmios request.
module Ctl.Internal.QueryM.EraSummaries
  ( getEraSummaries
  ) where

import Prelude

import Cardano.Types.EraSummaries (EraSummaries)
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM.Ogmios (eraSummaries) as Ogmios
import Ctl.Internal.QueryM.Ogmios.Types (pprintOgmiosDecodeError)
import Data.Either (Either(Right, Left))
import Data.Newtype (unwrap)
import Effect.Exception (error)

-- | Get `EraSummaries` as used for Slot arithemetic. Details can be found
-- | https://ogmios.dev/api/ under "eraSummaries" query
getEraSummaries :: QueryM EraSummaries
getEraSummaries = do
  resp <- Ogmios.eraSummaries
  case resp of
    Left err -> throwError $ error $ pprintOgmiosDecodeError err
    Right val -> pure $ unwrap $ val
