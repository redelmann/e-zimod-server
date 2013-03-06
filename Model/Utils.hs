
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
computeEnergy = undefined
