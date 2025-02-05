-- | A module to get "currentEpoch" via an Ogmios request.
module Ctl.Internal.QueryM.CurrentEpoch
  ( getCurrentEpoch
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM.JsonRpc2 (pprintOgmiosDecodeError)
import Ctl.Internal.QueryM.Ogmios (CurrentEpoch)
import Ctl.Internal.QueryM.OgmiosHttp (currentEpoch) as OgmiosHttp
import Data.Either (Either(Right, Left))
import Effect.Exception (error)

-- | Get the current Epoch. Details can be found https://ogmios.dev/api/ under
-- | "currentEpoch" query
getCurrentEpoch :: QueryM CurrentEpoch
getCurrentEpoch = do
  resp <- OgmiosHttp.currentEpoch
  case resp of
    Left err -> throwError $ error $ pprintOgmiosDecodeError err
    Right val -> pure val
