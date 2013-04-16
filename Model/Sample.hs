{-# LANGUAGE OverloadedStrings #-}

module Model.Sample where

import Control.Applicative ((<*>), (<$>))
import Control.Monad (mzero)

import Data.Aeson

import Model.Types
import Model.Profile

data Sample = Sample Second [Joule]
               deriving (Eq, Show)

instance ToJSON Sample where
    toJSON (Sample t xs) = object ["time" .= t, "data" .= xs]

instance FromJSON Sample where
    parseJSON (Object v) = Sample <$> v .: "time" <*> v .: "data"
    parseJSON _ = mzero

computeSample :: Second -> Profile -> Sample
computeSample t p = Sample t $ go 0 p
  where
    go :: Rational -> Profile -> [Joule]
    go i p' = x : go (i + 1) n'
      where
        t0 = i * t
        t1 = t0 + t
        (n, n') = split p' t1
        x       = computeEnergy t0 t1 n
