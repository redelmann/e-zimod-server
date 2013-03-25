

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
computeSample t p = Sample $ go p
  where
    go :: Profile -> [Joule]
    go p' = x : go (n' `deferredBy` (-t))
      where
        (n, n') = split p' t
        x       = computeEnergy 0 t n
