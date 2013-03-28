-- | This modules defines `Profile`s, which are power over time graphs.
module Model.Profile
    ( Profile
    -- * Smart constructors
    , mkProfile
    , constant
    , square
    -- * Transformations
    , split
    , andThen
    , upTo
    , deferredBy
    , clean
    -- * Computations
    , peek
    , peekr
    , peekl
    , computeEnergy
    , getList
    ) where

import Data.Ord
import Data.Aeson
import Data.List (sortBy)
import Control.Monad (mzero)
import Control.Applicative
import Control.Arrow (first, (&&&))

import Model.Types
import Utils.DBClass

{- | Power profile. Determines the power at some given time.

     The power between two points is set to be linear,
     and at both ends to be constant. -}
newtype Profile = Profile [(Second, Watt)]
               -- ^ Non empty, sorted by increasing second
               deriving (Read, Show, Eq)

instance DBisable Profile where
  -- | Serializes a profile, for use in the database.
  serialize = show
  -- | Deserializes a profile from a string.
  deserialize = read

instance FromJSON Profile where
    parseJSON json = do
        xs <- parseJSON json
        case mkProfile xs of
            Just p  -> return p
            Nothing -> mzero

instance ToJSON Profile where
    toJSON (Profile xs) = toJSON xs

{- | Profile constructor.
     Garantees that the list of events is non empty and sorted. -}
mkProfile :: [(Second, Watt)] -> Maybe Profile
mkProfile [] = Nothing
mkProfile xs = Just $ clean $ Profile $ sortBy (comparing fst) xs

-- | Constant profile.
constant :: Watt -> Profile
constant w = Profile [(0, w)]

-- | Square profile.
square :: Watt -> Second -> Watt -> Second -> Profile
square w1 dt1 w2 dt2 = Profile
    [(0, w1), (dt1, w1), (dt1, w2), (dt1 + dt2, w2)]

-- | Removes useless points.
clean :: Profile -> Profile
clean (Profile xs) = Profile $ cleanSameWatt $ cleanSameTime xs
  where
    cleanSameTime ((t1, v1) : (_, _) : (t2, v2) : tvs)
        | t1 == t2 = cleanSameTime $ (t1, v2) : (t2, v2) : tvs
    cleanSameTime (tv : tvs) = tv : cleanSameTime tvs
    cleanSameTime [] = []

    cleanSameWatt ((t1, v1) : (_, v2) : (t3, v3) : tvs)
        | v1 == v2 && v2 == v3 = cleanSameWatt $ (t1, v1) : (t3, v3) : tvs
    cleanSameWatt (tv : tvs) = tv : cleanSameWatt tvs
    cleanSameWatt [] = []

{- | Splits a profile in two, the first one being equivalent
     to the original upto time `t`, and the second one being equivalent to
     the original profile from time `t`. -}
split :: Profile -> Second -> (Profile, Profile)
split p@(Profile xs) t = (Profile as, Profile bs)
  where
    (as', bs') = break ((>= t) . fst) xs
    (a', b')   = peek p t
    as         = as' ++ [(t, a')]
    bs         = (t, b') : bs'

{- | Combines the points of two profiles.

     All points of the first profile must be
     before the first point of the second profile.
     This condition is not checked. -}
andThen :: Profile -> Profile -> Profile
andThen (Profile xs) (Profile ys) = Profile $ xs ++ ys

-- | Returns a profile equivalent up to a certain time and constant afterwards.
upTo :: Profile -> Second -> Profile
upTo pa@(Profile as) t = Profile $ as' ++ [(t, w)]
  where
    as' = takeWhile ((< t) . fst) as
    w   = peekl pa t

-- | Defers a profile by some time.
deferredBy :: Profile -> Second -> Profile
deferredBy (Profile xs) t = Profile $ map (first (+ t)) xs

-- | Evaluates a profile at a given time. Returns the left and right limits.
peek :: Profile -> Second -> (Watt, Watt)
peek p = peekl p &&& peekr p

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
computeEnergy :: Second -> Second -> Profile -> Joule
computeEnergy a b _ | b <= a = 0
computeEnergy a b p@(Profile xs) = snd $ foldr f ((b, peekl p b), 0) $
    (a, peekr p a) : ys
  where
    ys :: [(Second, Watt)]
    ys = takeWhile ((< b) . fst) $ dropWhile ((<= a) . fst) xs

    f :: (Second, Watt) -> ((Second, Watt), Joule) -> ((Second, Watt), Joule)
    f (t1, v1) ((t2, v2), j) = ((t1, v1), j + (t2 - t1) * (v1 + v2) / 2)

-- | Returns the list of `(Second, Watt)` pairs defining this profile.
getList :: Profile -> [(Second, Watt)]
getList (Profile xs) = xs
