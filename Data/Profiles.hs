
module Data.Profiles where

import Model

fridge :: MachineDescription
fridge = MachineDescription
    { name = "Philipp G. Fridge"
    , behavior = [ ("On", Repeat (square 0 2 100 2))
                 , ("Standby", Once $ constant 15)
                 , ("Off", Once $ constant 0) ]
    , transitions = [ ("On", "Off", 1, 0)
                    , ("Off", "On", 0, 10)
                    , ("On", "Standby", 5, 10)
                    , ("Standby", "On", 5, 10)
                    , ("Standby", "Off", 1, 0)
                    , ("Off", "Standby", 2, 10) ] }
