-- | This module provides type synonyms used by models.
module Model.Types (
    -- * SI units
      Watt
    , Second
    , Joule
    ) where

-- | Unit of power.
type Watt = Rational
-- | Unit of energy.
type Joule = Rational
-- | Unit of time.
type Second = Rational
