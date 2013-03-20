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
import Data.List (find, foldl')
import Data.Aeson

import Model.Types
import Model.Profile

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
    snd <$> foldl' f (Just (is, ip)) xs
  where
    f :: Maybe (State, Profile) -> (Second, State) -> Maybe (State, Profile)
    f Nothing _ = Nothing
    f (Just (s1, pa)) (t, s2) = do
        (_, _, dt) <- find (\ (a, b, _) -> a == s1 && b == s2) (transitions md)
        cp <- lookup s2 (behavior md)
        let pb = uncycle cp
        return (s2, transitionTo pa pb t dt)

{- | Represents possibly cyclic elements.

     This is used to allow serializing
     infinite cyclic structures as finite ones. -}
data Cyclic a = Repeat a  -- ^ The element is repeated ad infinitum
              | Once a    -- ^ The element is not repeated
              deriving (Show, Eq, Read)

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
uncycle (Repeat p) = unsafeMkProfile ys
  where
    xs = getList p
    ys = xs ++ map (first (+ lx)) ys
    lx = fst $ last xs

instance Functor Cyclic where
    fmap f (Once x) = Once (f x)
    fmap f (Repeat x) = Repeat (f x)
