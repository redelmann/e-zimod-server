
module Model.Machine
    ( MachineDescription (..)
    , MachineConfiguration (..)
    , State
    , Event
    , TimedEvent
    ) where

import Model.Types
import Model.Profile

type Event = String
type State = String
type TimedEvent = (Second, Event)

data MachineConfiguration = MachineConfiguration
    { state    :: State    -- ^ Current state of the machine
    , behavior :: Profile  -- ^ Consumption profile in this configuration
    }


data MachineDescription = MachineDescription
    { name       :: String  -- ^ Name of the machine
    , category   :: String  -- ^ Type of machine
    , transition :: TimedEvent  -- ^ Trigger event of the transition
                 -> MachineConfiguration
                 -- ^ Configuration when the event triggered
                 -> Maybe MachineConfiguration -- ^ Resulting configuration
    }
