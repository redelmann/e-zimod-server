
module Simulation.Parameter where

import Data.Map (Map)

import Model

data Parameter = Parameter
    { userProfile :: UserProfile
    , machines    :: Map Id MachineDescription
    , sampleTime  :: Second
    , untilTime   :: Second }
