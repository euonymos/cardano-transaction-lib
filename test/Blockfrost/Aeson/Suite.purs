module Test.Ctl.Blockfrost.Aeson.Suite (main, suite) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError
  , decodeAeson
  , parseJsonStringToAeson
  , printJsonDecodeError
  )
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Ctl.Internal.Service.Blockfrost
  ( BlockfrostChainTip
  , BlockfrostCurrentEpoch
  , BlockfrostEraSummaries
  , BlockfrostMetadata
  , BlockfrostNativeScript
  , BlockfrostProtocolParameters
  , BlockfrostScriptInfo
  , BlockfrostSystemStart
  )
import Data.Array (catMaybes, length)
import Data.Array.NonEmpty (tail)
import Data.Bifunctor (bimap, lmap)
import Data.Bounded.Generic (genericBottom)
import Data.Either (Either, hush)
import Data.Enum.Generic (genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Mote (group, test)
import Mote.TestPlanM (TestPlanM, interpret)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath, basename, concat)
import Type.Proxy (Proxy(Proxy))

main :: Effect Unit
main = launchAff_ (interpret suite)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Blockfrost Aeson tests" (tests (Just genericBottom))
  where
  tests :: Maybe Query -> TestPlanM (Aff Unit) Unit
  tests Nothing = pure unit
  tests (Just query) = do
    fixtures <- lift $ loadFixtures (printQuery query)
    test (printQuery query <> " (" <> show (length fixtures) <> ")") do
      for_ fixtures \{ aeson, bn } -> do
        let
          handle :: forall (a :: Type). DecodeAeson a => Proxy a -> Aff Unit
          handle _ = liftEither $ bimap
            (error <<< ((bn <> "\n ") <> _) <<< printJsonDecodeError)
            (const unit)
            (decodeAeson aeson :: Either JsonDecodeError a)
        case query of
          GetChainTipQuery ->
            handle (Proxy :: Proxy BlockfrostChainTip)
          GetCurrentEpochQuery ->
            handle (Proxy :: Proxy BlockfrostCurrentEpoch)
          GetEraSummariesQuery ->
            handle (Proxy :: Proxy BlockfrostEraSummaries)
          GetNativeScriptByHashQuery ->
            handle (Proxy :: Proxy BlockfrostNativeScript)
          GetProtocolParametersQuery ->
            handle (Proxy :: Proxy BlockfrostProtocolParameters)
          GetScriptInfoQuery ->
            handle (Proxy :: Proxy BlockfrostScriptInfo)
          GetSystemStartQuery ->
            handle (Proxy :: Proxy BlockfrostSystemStart)
          GetTxMetadataQuery ->
            handle (Proxy :: Proxy BlockfrostMetadata)
    tests (genericSucc query)

data Query
  = GetChainTipQuery
  | GetCurrentEpochQuery
  | GetEraSummariesQuery
  | GetNativeScriptByHashQuery
  | GetProtocolParametersQuery
  | GetScriptInfoQuery
  | GetSystemStartQuery
  | GetTxMetadataQuery

derive instance Generic Query _

printQuery :: Query -> String
printQuery = case _ of
  GetChainTipQuery -> "getChainTip"
  GetCurrentEpochQuery -> "getCurrentEpoch"
  GetEraSummariesQuery -> "getEraSummaries"
  GetNativeScriptByHashQuery -> "getNativeScriptByHash"
  GetProtocolParametersQuery -> "getProtocolParameters"
  GetScriptInfoQuery -> "getScriptInfo"
  GetSystemStartQuery -> "getSystemStart"
  GetTxMetadataQuery -> "getTxMetadata"

loadFixtures :: FilePath -> Aff (Array { aeson :: Aeson, bn :: String })
loadFixtures query = do
  files <- readdir' path
  catMaybes <$> flip parTraverse files \filepath -> do
    let bn = basename filepath
    case pattern >>= flip match bn >>> map tail of
      Just [ Just query' ] | query' == query -> do
        contents <- readTextFile UTF8 filepath
        aeson <- liftEither $ lmap
          (error <<< ((bn <> "\n ") <> _) <<< printJsonDecodeError)
          (parseJsonStringToAeson contents)
        pure $ Just { aeson, bn }
      _ -> pure Nothing
  where
  path :: FilePath
  path = concat [ "fixtures", "test", "blockfrost", query ]

  pattern :: Maybe Regex
  pattern = hush $ regex "^([a-zA-Z]+)-[0-9a-fA-F]+\\.json$" noFlags

readdir' :: FilePath -> Aff (Array FilePath)
readdir' fp = (map <<< map) (\fn -> concat [ fp, fn ]) (readdir fp)
