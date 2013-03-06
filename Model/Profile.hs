
module Model.Profile(Profile(..)) where

import Model.Types

data Profile = Constant Watt
             | Linear (Second, Watt) Rational
             | Add Profile Profile
             | Mult Rational Profile
             | LinearInterporlation [(Second, Watt)]