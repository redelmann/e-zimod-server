
module Simulation.MonteCarlo (optimize) where

import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Random
import Data.List (minimumBy, foldl')
import Data.Ord (comparing)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Model
import Simulation.Parameter

optimize :: Int -> Parameter -> Rand UserProfile
optimize tries params = minimumBy (comparing score) .
                        (userProfile params :) <$>
                        replicateM tries generate
  where
    descriptionAndUsages :: M.Map Name (MachineDescription, MachineUsage)
    descriptionAndUsages = M.intersectionWith (,) 
        (machines params) (usages $ userProfile params)

    score :: UserProfile -> Rational
    score up = maximum $ take steps $ sampledValues $ foldr1 merge $
               map (computeSample $ sampleTime params) $
               map (\ (md, mu) -> 
                    fromJust $ computeProfile md (initially mu) (usage mu)) $
               concretize up (machines params)
    
    steps :: Int
    steps = ceiling (sampleTime params / untilTime params)

    generate :: Rand UserProfile
    generate = UserProfile <$> mapM delayUsage descriptionAndUsages

    delays :: [(State, State, Second, Second)]
           -> Rand (M.Map (State, State) Second)
    delays xs = M.fromList <$> mapM (\ (a, b, _, s) -> 
        ((,) (a, b) . toRational <$> inRange (0, floor s :: Int))) xs

    delayUsage :: (MachineDescription, MachineUsage) -> Rand MachineUsage
    delayUsage (md, MachineUsage s us) = do
        ds <- delays (transitions md)
        return $ MachineUsage s $ correctTime $ reverse $
                 snd $ foldl' (go ds) (s, []) us
      where
        go :: (M.Map (State, State) Second)
           -> (State, [(Second, State)]) 
           -> (Second, State) 
           -> (State, [(Second, State)])
        go ds (s, acc) (t, s') = 
            let Just dt = M.lookup (s, s') ds in (s', (t+dt, s'):acc)

        correctTime :: [(Second, State)] -> [(Second, State)]
        correctTime ((t1, s1) : ts@((t2, s2) : _)) =
            (min t1 t2, s1) : correctTime ts
        correctTime xs = xs

