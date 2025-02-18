-- | Arbitrary precision rational numbers (backed by `BigInt`).
module Contract.Numeric.Rational
  ( module Rational
  ) where

import Ctl.Internal.Types.Rational
  ( class RationalComponent
  , Rational
  , denominator
  , numerator
  , recip
  , reduce
  , (%)
  ) as Rational
