
module Data.Profiles where

import Model

fridge :: MachineDescription
fridge = MachineDescription
    { name = "Philipp G. Fridge"
    , behavior = [ ("On", Repeat (square 0 2 100 2))
                 , ("Standby", Once $ constant 15)
                 , ("Off", Once $ constant 0) ]
    , transitions = [ ("On", "Off", 1)
                    , ("Off", "On", 0)
                    , ("On", "Standby", 5)
                    , ("Standby", "On", 5)
                    , ("Standby", "Off", 1)
                    , ("Off", "Standby", 2) ] }
