
module Model.Profile
    ( Profile
    , mkProfile
    , peek
    , computeEnergy
    , serialize
    , deserialize
    ) where

import Data.Ord
import Data.Aeson
import Data.List (sortBy)
import Control.Monad (mzero)
import Control.Applicative

import Model.Types

-- | Power profile. Determines the power at some given time.
data Profile = Profile [(Second, Watt)]  -- Sorted by increasing second, and non empty.
               deriving (Show, Read)

instance FromJSON Profile where
    parseJSON json = do
        xs <- parseJSON json
        case mkProfile xs of
            Just p  -> return p
            Nothing -> mzero

instance ToJSON Profile where
    toJSON (Profile xs) = toJSON xs

-- | Profile constructor. Garantees that the list of events is non empty and sorted. 
mkProfile :: [(Second, Watt)] -> Maybe Profile
mkProfile [] = Nothing
mkProfile xs = Just $ Profile $ sortBy (comparing fst) xs

-- | Evaluates a profile at a given time.
peek :: Profile -> Second -> Watt
peek (Profile ((t1, v1):xs)) t | t <= t1 = v1  -- When time is less or equal to first.
peek (Profile xs) t = peek' xs
  where
    peek' ((t1, v1):[]) | t >= t1 = v1  -- When time is greater or equal to last.
    peek' ((t1, v1):rs@((t2, v2):_))
        | t1 <= t && t <= t2 = v1 + ((t - t1) / (t2 - t1)) * (v2 - v1)
        | otherwise = peek' rs

-- | Computes energy of a profile within a time period.
computeEnergy :: Rational -> Rational -> Profile -> Joule
computeEnergy a b _ | b <= a = 0
computeEnergy a b p@(Profile xs) = snd $ foldr f ((b, peek p b), 0) $ (a, peek p a) : ys
  where
    ys :: [(Second, Watt)]
    ys = takeWhile ((<b) . fst) $ dropWhile ((<=a) . fst) xs

    f :: (Second, Watt) -> ((Second, Watt), Joule) -> ((Second, Watt), Joule)
    f (t1, v1) ((t2, v2), j) = ((t1, v1), j + (t2 - t1) * (v1 + v2) / 2)


serialize :: Profile -> String
serialize = show

deserialize :: String -> Profile
deserialize = read
