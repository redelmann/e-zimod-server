
module Simulation.Parameter where

import Data.Map (Map)

import Model

data Parameter = Parameter 
    { userProfile :: UserProfile
    , machines    :: Map Name MachineDescription
    , sampleTime  :: Second
    , untilTime   :: Second }