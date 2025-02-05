-- | A module to get "currentEpoch" via an Ogmios request.
module Ctl.Internal.QueryM.CurrentEpoch
  ( getCurrentEpoch
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM.Ogmios (currentEpoch) as Ogmios
import Ctl.Internal.QueryM.Ogmios.Types (CurrentEpoch, pprintOgmiosDecodeError)
import Data.Either (Either(Right, Left))
import Effect.Exception (error)

-- | Get the current Epoch. Details can be found https://ogmios.dev/api/ under
-- | "currentEpoch" query
getCurrentEpoch :: QueryM CurrentEpoch
getCurrentEpoch = do
  resp <- Ogmios.currentEpoch
  case resp of
    Left err -> throwError $ error $ pprintOgmiosDecodeError err
    Right val -> pure val
