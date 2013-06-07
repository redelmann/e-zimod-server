{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}

module Model.Sample
    ( Sample
    , computeSample
    , computeMaximums
    , takeOnly
    , merge
    , sampledValues
    ) where

import Control.Applicative ((<*>), (<$>))
import Control.Monad (mzero)

import Database.HDBC
import Data.Convertible.Base
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as B
import Data.Aeson

import Model.Types
import Model.Profile

data Sample = Sample Second [Joule]
               deriving (Eq, Show)

instance B.Binary Sample where
    put (Sample s js) = do
        B.put s
        B.put js

    get = Sample <$> B.get <*> B.get

instance Convertible Sample SqlValue where
    safeConvert = Right . SqlByteString . BS.concat . BL.toChunks . B.encode

instance Convertible SqlValue Sample where
    safeConvert (SqlByteString bs) = Right $ B.decode $ BL.fromChunks [bs]

instance ToJSON Sample where
    toJSON (Sample t xs) = object ["time" .= t, "data" .= xs]

instance FromJSON Sample where
    parseJSON (Object v) = Sample <$> v .: "time" <*> v .: "data"
    parseJSON _ = mzero

takeOnly :: Int -> Sample -> Sample
takeOnly i (Sample k xs) = Sample k $ take i xs

merge :: Sample -> Sample -> Sample
merge (Sample t xs) (Sample t' ys) | t == t' = Sample t $ zipWith (+) xs ys

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

computeMaximums :: Second -> Profile -> [Watt]
computeMaximums t p = go 0 p
  where
    go :: Rational -> Profile -> [Watt]
    go i p' = x : go (i + 1) n'
      where
        t0 = i * t
        t1 = t0 + t
        (n, n') = split p' t1
        x       = maximum $ map snd $ getList n

sampledValues :: Sample -> [Joule]
sampledValues (Sample _ vs) = vs
