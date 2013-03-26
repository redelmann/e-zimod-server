

module Model.Sample where

import Data.Aeson

import Model.Types
import Model.Profile

newtype Sample = Sample [Joule]
	deriving (Eq, Show)

instance ToJSON Sample where
	toJSON (Sample xs) = toJSON xs

instance FromJSON Sample where
	parseJSON = fmap Sample . parseJSON

computeSample :: Second -> Profile -> Sample
computeSample t p = Sample $ go 0 p
  where
  	go :: Rational -> Profile -> [Joule]
  	go i p' = x : go (i+1) n'
  	  where
  	  	t0 = i * t
  	  	t1 = t0 + t
  	  	(n, n') = split p' t1
  	  	x       = computeEnergy t0 t1 n

