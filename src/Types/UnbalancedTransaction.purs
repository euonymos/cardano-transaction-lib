module Types.UnbalancedTransaction where

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Prelude
import Types.ByteArray (ByteArray(..))
import Types.POSIXTimeRange (POSIXTimeRange)
import Types.Transaction (DataHash, Ed25519KeyHash(..), Transaction, TransactionInput, Value)

newtype PubKey = PubKey ByteArray

derive instance Newtype PubKey _
derive newtype instance Eq PubKey

newtype PaymentPubKey = PaymentPubKey PubKey

derive instance Newtype PaymentPubKey _
derive newtype instance Eq PaymentPubKey

newtype ValidatorHash = ValidatorHash ByteArray

derive instance Newtype ValidatorHash _
derive newtype instance Eq ValidatorHash

newtype ScriptOutput = ScriptOutput
  { validatorHash :: ValidatorHash
  , value :: Value
  , datumHash :: DataHash
  }

derive instance Newtype ScriptOutput _

newtype PubKeyHash = PubKeyHash Ed25519KeyHash

derive instance Newtype PubKeyHash _
derive newtype instance Eq PubKeyHash
derive newtype instance Ord PubKeyHash

newtype PaymentPubKeyHash = PaymentPubKeyHash PubKeyHash

derive instance Newtype PaymentPubKeyHash _
derive newtype instance Eq PaymentPubKeyHash
derive newtype instance Ord PaymentPubKeyHash

-- | Transaction inputs reference some other transaction's outputs.
type TxOutputRef = TransactionInput

-- | An unbalanced transaction. It needs to be balanced and signed before it
-- | can be submitted to the ledeger.
-- | Resembles `UnbalancedTx` from `plutus-apps`.
newtype UnbalancedTx = UnbalancedTx
  { transaction :: Transaction
  , requiredSignatories :: Map PaymentPubKeyHash (Maybe PaymentPubKey)
  , utxoIndex :: Map TxOutputRef ScriptOutput
  , validityTimeRange :: POSIXTimeRange
  }

derive instance Newtype UnbalancedTx _
