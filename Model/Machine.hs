
module Model.Machine
    ( MachineDescription (..)
    , Cyclic (..)
    , State
    ) where

import Model.Types
import Model.Profile

type State = String

data Cyclic a = Repeat a
              | Once a
              deriving (Show, Eq, Read)

data MachineDescription = MachineDescription
    { name        :: String  -- ^ Name of the machine
    , behavior    :: [(State, Cyclic Profile)]
    , transitions :: [(State, State, Second)]
    } deriving (Show, Eq, Read)
