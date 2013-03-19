
module Model.Machine
    ( MachineDescription (..)
    , Cyclic (..)
    , State
    , computeProfile
    , uncycle
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.List (find)

import Model.Types
import Model.Profile

type State = String

data Cyclic a = Repeat a
              | Once a
              deriving (Show, Eq, Read)

instance Functor Cyclic where
    fmap f (Once x) = Once (f x)
    fmap f (Repeat x) = Repeat (f x)

data MachineDescription = MachineDescription
    { name        :: String  -- ^ Name of the machine
    , behavior    :: [(State, Cyclic Profile)]
    , transitions :: [(State, State, Second)]
    } deriving (Show, Eq, Read)

uncycle :: Cyclic Profile -> Profile
uncycle (Once p) = p
uncycle (Repeat p) = unsafeMkProfile ys
  where
    xs = getList p
    ys = xs ++ map (first (+ lx)) ys
    lx = fst $ last xs


computeProfile :: MachineDescription
               -> State
               -> [(Second, State)]
               -> Maybe Profile
computeProfile md is [] = uncycle <$> lookup is (behavior md)
computeProfile md is xs = do
    ip <- uncycle <$> lookup is (behavior md)
    snd <$> foldl f (Just (is, ip)) xs
  where
    f :: Maybe (State, Profile) -> (Second, State) -> Maybe (State, Profile)
    f Nothing _ = Nothing
    f (Just (s1, pa)) (t, s2) = do
        (_, _, dt) <- find (\ (a, b, _) -> a == s1 && b == s2) (transitions md)
        cp <- lookup s2 (behavior md)
        let pb = uncycle cp
        return (s2, transitionTo pa pb t dt)
