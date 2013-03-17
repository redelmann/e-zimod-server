module Main(main) where

import Test.Utils
import Test.QuickCheck

import Data.Maybe (isJust)

import Model.Types
import Model.Profile

profile1 :: Bool
profile1 = not $ isJust $ mkProfile []

profile2 :: NonEmptyList (Second, Watt) -> Bool
profile2 = isJust . mkProfile . getNonEmpty 

peekr1 :: Rational -> Rational -> Bool
peekr1 r t = withProfile [(0, r)] $ \ p -> peekr p t == r

peekr2 :: Bool
peekr2 = withProfile [(0, 10), (10, 20)] $ \ p -> all id $
    [ peekr p (-20) == 10
    , peekr p 0 == 10
    , peekr p 5 == 15
    , peekr p 2 == 12
    , peekr p 10 == 20 
    , peekr p 42 == 20 ]

peekr3 :: Watt -> Watt -> Second -> [(Second, Watt)] -> Bool
peekr3 v1 v2 t xs = withProfile (xs ++ [(t, v1), (t, v2)]) $ \ p ->
    peekr p t == v2

peekl1 :: Rational -> Rational -> Bool
peekl1 r t = withProfile [(0, r)] $ \ p -> peekl p t == r

peekl2 :: Bool
peekl2 = withProfile [(0, 10), (10, 20)] $ \ p -> all id $
    [ peekl p (-20) == 10
    , peekl p 0 == 10
    , peekl p 5 == 15
    , peekl p 2 == 12
    , peekl p 10 == 20 
    , peekl p 42 == 20 ]

peekl3 :: Watt -> Watt -> Second -> [(Second, Watt)] -> Bool
peekl3 v1 v2 t xs = withProfile ([(t, v1), (t, v2)] ++ xs) $ \ p ->
    peekl p t == v1

computeEnergy1 :: Second -> Second -> NonEmptyList (Second, Positive Watt) -> Bool
computeEnergy1 a b xs = withProfile 
    (map (\ (s, pw) -> (s, getPositive pw)) $ getNonEmpty xs)
    ((>=0) . computeEnergy a b)

computeEnergy2 :: Second -> Positive Second -> Watt -> Watt -> Bool
computeEnergy2 t0 dt w0 w1 = withProfile [(t0, w0), (t0, w1), (t1, w1), (t1, w0)] $ \ p ->
    computeEnergy t0 t1 p == (t1 - t0) * w1 
  where
    t1 = t0 + getPositive dt

main :: IO ()
main = tests
    [ "Profile construction without any point"                                  <~> profile1
    , "Profile construction with at least one point"                            <~> profile2
    , "Peekr on constant profile"                                               <~> peekr1
    , "Peekr on two points profile"                                             <~> peekr2
    , "Peekr on same valued elements returns the last (rightmost) in the list." <~> peekr3
    , "Peekl on constant profile"                                               <~> peekl1
    , "Peekl on two points profile"                                             <~> peekl2
    , "Peekl on same valued elements returns the first (leftmost) in the list." <~> peekl3
    , "Energy always positive or null if profile is always positive."           <~> computeEnergy1 
    , "Energy is computed correctly in case of values occuring at same time."   <~> computeEnergy2 ]