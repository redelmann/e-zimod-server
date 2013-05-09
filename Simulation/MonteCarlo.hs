
-- | This module provides simulation functions based on the Monte Carlo method.
module Simulation.MonteCarlo (optimize) where

import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Random
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Model
import Simulation.Parameter

{- | Tries various combinations of delays
     in order to decrease consumption peeks. -}
optimize :: Int  -- ^ Number of user profiles to generate and test.
         -> Parameter  -- ^ Simulation parameters.
         -> Rand UserProfile
optimize tries params =
    -- Get the profile with least score
    minimumBy (comparing score) .
    -- Adds the original user profile to the list of contenders
    (userProfile params :) <$>
    -- Generates the given number of user profiles
    replicateM tries generate
  where
    -- Binds togethers machine descriptions and usages of the same name.
    descriptionAndUsages :: M.Map Name (MachineDescription, MachineUsage)
    descriptionAndUsages = M.intersectionWith (,)
        (machines params) (usages $ userProfile params)

    -- Computes the score of a user profile.
    score :: UserProfile -> Rational
    score up =
        -- Take the maximum of the values relevant up to that point in time
        maximum $ take steps $ sampledValues $
        -- Merge all samples, resulting an overall consumption
        foldr1 merge $
        -- Given the exact profiles, compute the samples
        map (computeSample (sampleTime params) .
        -- Compute the profile for each machine according to its usage
             (\ (md, mu) ->
                fromJust $ computeProfile md (initially mu) (usage mu))) $
        -- Match the machine descriptions with their usages
        concretize up (machines params)

    -- Number of samples needed.
    steps :: Int
    steps = ceiling (sampleTime params / untilTime params)

    {- Generates a user profile, postponing events randomly,
       respecting the constaints. -}
    generate :: Rand UserProfile
    generate = UserProfile <$> mapM delayUsage descriptionAndUsages

    -- Computes the a random delay generator for each transition.
    delays :: [(State, State, Second, Second)]
           -> M.Map (State, State) (Rand Second)
    delays xs = M.fromList $ map (\ (a, b, _, s) ->
        ((a, b), toRational <$> inRange (0, floor s :: Int))) xs

    {- Given a machine description and an original use of it,
       generates a random valid new usage. -}
    delayUsage :: (MachineDescription, MachineUsage) -> Rand MachineUsage
    delayUsage (md, MachineUsage s us) =
        MachineUsage s . correctTime . reverse . snd <$>
        foldM go (s, []) us
      where
        -- Get delay generator for this machine
        ds :: M.Map (State, State) (Rand Second)
        ds = delays $ transitions md

        go :: (State, [(Second, State)])
           -> (Second, State)
           -> Rand (State, [(Second, State)])
        go (s, acc) (t, s') = do
            let Just randDelay = M.lookup (s, s') ds
            dt <- randDelay
            return (s', (t + dt, s') : acc)

        -- | Ensures the correct order of events.
        correctTime :: [(Second, State)] -> [(Second, State)]
        correctTime ((t1, s1) : ts@((t2, s2) : _)) =
            (min t1 t2, s1) : correctTime ts
        correctTime xs = xs
