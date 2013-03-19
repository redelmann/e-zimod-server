
module Model.Profile
    ( Profile
    , mkProfile
    , unsafeMkProfile
    , peekr
    , peekl
    , transitionTo
    , computeEnergy
    , getList
    , constant
    , square
    , serialize
    , deserialize
    ) where

import Data.Ord
import Data.Aeson
import Data.List (sortBy)
import Control.Monad (mzero)
import Control.Applicative
import Control.Arrow (first)

import Model.Types

-- | Power profile. Determines the power at some given time.
data Profile = Profile [(Second, Watt)]  {- Non empty,
                                            sorted by increasing second -}
               deriving (Show, Read, Eq)

instance FromJSON Profile where
    parseJSON json = do
        xs <- parseJSON json
        case mkProfile xs of
            Just p  -> return p
            Nothing -> mzero

instance ToJSON Profile where
    toJSON (Profile xs) = toJSON xs

getList :: Profile -> [(Second, Watt)]
getList (Profile xs) = xs

{- | Profile constructor.
     Garantees that the list of events is non empty and sorted. -}
mkProfile :: [(Second, Watt)] -> Maybe Profile
mkProfile [] = Nothing
mkProfile xs = Just $ Profile $ clean $ sortBy (comparing fst) xs
  where
    {- Removes elements occuring between two elements occuring at the same time,
       only keeping right and left limits. -}
    clean ((t1, v1) : (_, _) : (t2, v2) : tvs)
        | t1 == t2 = clean $ (t1, v2) : (t2, v2) : tvs
    clean (tv : tvs) = tv : clean tvs
    clean [] = []

unsafeMkProfile :: [(Second, Watt)] -> Profile
unsafeMkProfile = Profile

transitionTo :: Profile -> Profile -> Second -> Second -> Profile
transitionTo pa@(Profile as) (Profile bs) t dt = Profile ys
  where
    as' = takeWhile ((< t) . fst) as
    w   = peekl pa t
    t'  = t + dt
    bs' = map (first (+ t')) bs
    ys  = as' ++ (t, w) : bs'


-- | Evaluates a profile at a given time. Takes the limit coming from the right.
peekr :: Profile -> Second -> Watt
-- When time is less to first.
peekr (Profile ((t1, v1) : xs)) t | t < t1 = v1
peekr (Profile xs) t = peek' xs
  where
    -- When time is greater or equal to last.
    peek' ((t1, v1) : []) | t >= t1 = v1
    peek' ((t1, v1) : rs@((t2, v2) : _))
        | t1 <= t && t < t2 = v1 + ((t - t1) / (t2 - t1)) * (v2 - v1)
        | otherwise = peek' rs

-- | Evaluates a profile at a given time. Takes the limit coming from the left.
peekl :: Profile -> Second -> Watt
-- When time is less or equal to first.
peekl (Profile ((t1, v1) : xs)) t | t <= t1 = v1
peekl (Profile xs) t = peek' xs
  where
    -- When time is greater or equal to last.
    peek' ((t1, v1) : []) | t >= t1 = v1
    peek' ((t1, v1) : rs@((t2, v2) : _))
        | t1 <= t && t <= t2 = v1 + ((t - t1) / (t2 - t1)) * (v2 - v1)
        | otherwise = peek' rs

-- | Computes energy of a profile within a time period.
computeEnergy :: Rational -> Rational -> Profile -> Joule
computeEnergy a b _ | b <= a = 0
computeEnergy a b p@(Profile xs) = snd $ foldr f ((b, peekl p b), 0) $
    (a, peekr p a) : ys
  where
    ys :: [(Second, Watt)]
    ys = takeWhile ((< b) . fst) $ dropWhile ((<= a) . fst) xs

    f :: (Second, Watt) -> ((Second, Watt), Joule) -> ((Second, Watt), Joule)
    f (t1, v1) ((t2, v2), j) = ((t1, v1), j + (t2 - t1) * (v1 + v2) / 2)

-- | Constant profile.
constant :: Watt -> Profile
constant w = Profile [(0, w)]

-- | Square profile.
square :: Watt -> Second -> Watt -> Second -> Profile
square w1 dt1 w2 dt2 = Profile
    [(0, w1), (dt1, w1), (dt1, w2), (dt1 + dt2, w2)]

serialize :: Profile -> String
serialize = show

deserialize :: String -> Profile
deserialize = read
