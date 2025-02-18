module Ctl.Internal.Service.Helpers
  ( aesonArray
  , aesonString
  , aesonObject
  , aesonNull
  , decodeAssetClass
  ) where

import Prelude

import Aeson
  ( Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonArray
  , caseAesonNull
  , caseAesonObject
  , caseAesonString
  )
import Cardano.AsCbor (decodeCbor)
import Cardano.Types.AssetName (AssetName, mkAssetName)
import Cardano.Types.ScriptHash (ScriptHash)
import Control.Apply (lift2)
import Data.ByteArray (hexToByteArray)
import Data.Either (Either(Left), note)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Foreign.Object (Object)

aesonArray
  :: forall (a :: Type)
   . (Array Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonArray = caseAesonArray (Left (TypeMismatch "Array"))

aesonObject
  :: forall (a :: Type)
   . (Object Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonObject = caseAesonObject (Left (TypeMismatch "Object"))

aesonString
  :: forall (a :: Type)
   . (String -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonString = caseAesonString (Left (TypeMismatch "String"))

aesonNull
  :: forall (a :: Type)
   . Aeson
  -> Either JsonDecodeError Unit
aesonNull = caseAesonNull (Left (TypeMismatch "Null")) pure

decodeAssetClass
  :: String
  -> String
  -> String
  -> Either JsonDecodeError (ScriptHash /\ AssetName)
decodeAssetClass assetString csString tnString =
  lift2 Tuple
    ( note (assetStringTypeMismatch "CurrencySymbol" csString)
        (decodeCbor <<< wrap =<< hexToByteArray csString)
    )
    ( note (assetStringTypeMismatch "AssetName" tnString)
        (mkAssetName =<< hexToByteArray tnString)
    )
  where
  assetStringTypeMismatch :: String -> String -> JsonDecodeError
  assetStringTypeMismatch t actual =
    TypeMismatch $
      ("In " <> assetString <> ": Expected hex-encoded " <> t)
        <> (", got: " <> actual)
