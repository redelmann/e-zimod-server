
module Model.Utils
    ( computeProfile
    , computeEnergy
    ) where

import Model.Types
import Model.Machine
import Model.Profile

computeProfile :: MachineDescription   -- ^ The machine on which to apply the events
               -> MachineConfiguration -- ^ Initial machine configuration
               -> [TimedEvent]         -- ^ List of events
               -> Maybe Profile        -- ^ Resulting consumption profile
computeProfile = undefined

computeEnergy :: Rational -> Rational -> Profile -> Joule
computeEnergy a b (Constant x) = (b - a) * x
computeEnergy a b (Linear t w r) = (b - a) * w + r/2*(b - a)^2
computeEnergy a b (Add x y) = computeEnergy a b x + computeEnergy a b y
computeEnergy a b (Mult r p) = r * computeEnergy a b p
computeEnergy a b (LinearInterpolation xs) = undefined
