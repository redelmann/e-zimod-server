-- | This module provides type synonyms used by models.
module Model.Types (
    -- * SI units
      Watt
    , Second
    , Joule
    , KiloWattHour
    , jouleToKiloWattHour
    , kiloWattHourToJoule
    ) where

-- | Unit of time.
type Second = Rational

-- | Unit of power.
type Watt = Rational

-- | Unit of energy.
type Joule = Rational

-- | Unit of energy.
type KiloWattHour = Rational

-- | Converts from Joules to Kilo Watts per Hour.
jouleToKiloWattHour :: Joule -> KiloWattHour
jouleToKiloWattHour = (/ 3600000)

-- | Converts from Kilo Watts per Hour to Joules.
kiloWattHourToJoule :: KiloWattHour -> Joule
kiloWattHourToJoule = (* 3600000)
