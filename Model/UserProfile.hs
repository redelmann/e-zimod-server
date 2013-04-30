{-# LANGUAGE MultiParamTypeClasses #-}

module Model.UserProfile where

import qualified Data.Map as M
import qualified Data.Binary as B
import Data.Aeson
import Database.HDBC
import Data.Convertible.Base
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Control.Applicative

import Model.State
import Model.Types

type Name = String

newtype UserProfile = UserProfile (M.Map Name [(Second, State)])
	deriving (Eq, Show)

instance B.Binary UserProfile where
    put (UserProfile as) = B.put as
    get = UserProfile <$> B.get

instance Convertible UserProfile SqlValue where
    safeConvert = Right . SqlByteString . BS.concat . BL.toChunks . B.encode

instance Convertible SqlValue UserProfile where
    safeConvert (SqlByteString bs) = Right $ B.decode $ BL.fromChunks [bs]

instance ToJSON UserProfile where
    toJSON (UserProfile xs) = toJSON xs

instance FromJSON UserProfile where
    parseJSON v = UserProfile <$> parseJSON v