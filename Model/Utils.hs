
module Model.Utils
    ( computeProfile
    ) where

import Model.Types
import Model.Machine
import Model.Profile

computeProfile :: MachineDescription   -- ^ The machine on which events apply
               -> MachineConfiguration -- ^ Initial machine configuration
               -> [TimedEvent]         -- ^ List of events
               -> Maybe Profile        -- ^ Resulting consumption profile
computeProfile = undefined
