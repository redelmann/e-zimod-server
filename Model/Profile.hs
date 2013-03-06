
module Model.Profile
    (Profile(..)
    , peek
    ) where

import Control.Applicative

import Model.Types

-- | Power profile. Determines the power at some given time.
data Profile = Constant Watt
             | Linear Second Watt Rational
             | Add Profile Profile
             | Mult Rational Profile
             | LinearInterporlation [(Second, Watt)]

-- | Evaluate a profile at a gievn time.
peek :: Profile -> Second -> Maybe Watt
peek (Constant x) _ = return x
peek (Linear t1 w r) t2 = return $ w + (t2 - t1) * r
peek (Add x y) t = (+) <$> peek x t <*> peek y t
peek (Mult r p) t = (*r) <$> (peek p t)
peek (LinearInterporlation xs) t = f xs
  where
    f :: [(Second, Watt)] -> Maybe Watt
    f ((t1, v1):vs@((t2, v2):_)) 
        | t < t1    = Nothing
        | t > t2    = f vs
        | otherwise = return $ v1 + ((t - t1) / (t2 - t1)) * (v2 - v1)
    f _ = Nothing
