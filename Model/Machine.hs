{-# LANGUAGE OverloadedStrings #-}

-- | This modules defines creation and manipulation of machines.
module Model.Machine (
    -- * Machine description
      MachineDescription (..)
    , State
    -- * Computations
    , computeProfile
    -- * Cyclic behavior
    , Cyclic (..)
    , uncycle
    ) where

import Control.Monad (mzero)
import Control.Applicative
import Control.Arrow (first)
import Data.List (find, foldl', isPrefixOf)
import Data.Aeson

import Model.Types
import Model.Profile
import Utils.DBClass

-- | Type of possible states of machines.
type State = String

-- | Describes a machine.
data MachineDescription = MachineDescription
    { name        :: String
    -- ^ Name of the machine
    , behavior    :: [(State, Cyclic Profile)]
    -- ^ Behavior in various states, as described by possibly cyclic profiles
    , transitions :: [(State, State, Second)]
    -- ^ Transitions time between states
    } deriving (Show, Eq, Read)

instance FromJSON MachineDescription where
    parseJSON (Object v) = MachineDescription
        <$> v .: "name"
        <*> v .: "behavior"
        <*> v .: "transitions"
    parseJSON _ = mzero

instance ToJSON MachineDescription where
    toJSON (MachineDescription n b t) =
        object ["name" .= n, "behavior" .= b, "transitions" .= t]

{- | Given a machine, an initial state and
     a list of states to transition to, sorted by increasing time,
     computes the resulting power profile. -}
computeProfile :: MachineDescription  -- ^ The machine
               -> State               -- ^ The initial state of the machine
               -> [(Second, State)]   -- ^ List of states to transition to
               -> Maybe Profile
computeProfile md is xs = do
    ip <- uncycle <$> lookup is (behavior md)
    (clean . foldr1 andThen . reverse . snd) <$> foldl' f (Just (is, [ip])) xs
  where
    f :: Maybe (State, [Profile])
      -> (Second, State)
      -> Maybe (State, [Profile])
    f Nothing _ = Nothing
    f (Just (s1, p : ps)) (t, s2) = do
        -- Trimming the last computed profile upto time t.
        let pa = p `upTo` t
        -- Getting transition time.
        (_, _, dt) <- find (\ (a, b, _) -> a == s1 && b == s2) (transitions md)
        -- Getting behavior in new state.
        cp <- lookup s2 (behavior md)
        -- Uncycling the behavior.
        let pb = uncycle cp
        -- Returning the new states along with the new profiles.
        return (s2, pb `deferredBy` (t + dt) : pa : ps)

{- | Represents possibly cyclic elements.

     This is used to allow serializing
     infinite cyclic structures as finite ones. -}
data Cyclic a = Repeat a  -- ^ The element is repeated ad infinitum
              | Once a    -- ^ The element is not repeated
              deriving (Show, Eq, Read)

instance DBisable a => DBisable (Cyclic a) where
    serialize (Once x) = "O," ++ serialize x
    serialize (Repeat x) = "R," ++ serialize x
    deserialize str | "O," `isPrefixOf` str = Once (deserialize str)
                    | "R," `isPrefixOf` str = Repeat (deserialize str)
                    | otherwise = error $ "String ''" ++
                                          str ++
                                          "'' cannot be view as Cyclic object "

instance ToJSON a => ToJSON (Cyclic a) where
    toJSON (Once x)   = object ["cyclic" .= False, "data" .= x]
    toJSON (Repeat x) = object ["cyclic" .= True,  "data" .= x]

instance FromJSON a => FromJSON (Cyclic a) where
    parseJSON (Object v) = do
        c <- v .: "cyclic"
        d <- v .: "data"
        return $ if c then Once d else Repeat d
    parseJSON _ = mzero

-- | Computes the infinite profile corresponding to this cyclic profile.
uncycle :: Cyclic Profile -> Profile
uncycle (Once p) = p
uncycle (Repeat p) = p'
  where
    xs = getList p
    t = fst $ last xs
    p' = p `andThen` (p' `deferredBy` t)

instance Functor Cyclic where
    fmap f (Once x) = Once (f x)
    fmap f (Repeat x) = Repeat (f x)
