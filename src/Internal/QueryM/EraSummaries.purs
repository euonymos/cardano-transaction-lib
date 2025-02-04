-- | A module to get "eraSummaries" via an Ogmios request.
module Ctl.Internal.QueryM.EraSummaries
  ( getEraSummaries
  ) where

import Prelude

import Cardano.Types.EraSummaries (EraSummaries)
import Ctl.Internal.QueryM (QueryM, mkOgmiosRequest)
import Ctl.Internal.QueryM.Ogmios (queryEraSummariesCall) as Ogmios
import Data.Newtype (unwrap)

-- | Get `EraSummaries` as used for Slot arithemetic. Details can be found
-- | https://ogmios.dev/api/ under "eraSummaries" query
getEraSummaries :: QueryM EraSummaries
getEraSummaries =
  unwrap <$> mkOgmiosRequest Ogmios.queryEraSummariesCall _.eraSummaries unit
