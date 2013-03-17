module Main(main) where

import Test.Utils
import Test.QuickCheck

import Data.Int
import Data.Aeson

import Model.Types
import Model.Profile

-- | Due to JSON encoding every number as double, 
--   we lose precision in the translation. 
decodeEncode :: NonEmptyList (Int8, Int8) -> Bool
decodeEncode xs = withProfile (map (\ (s, i) -> (toRational s, toRational i)) $ getNonEmpty xs) $ \ p ->
	decode (encode p) == Just p

main :: IO ()
main = tests
	[ "Decode is inverse of encode" <~> decodeEncode ]