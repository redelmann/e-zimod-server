{-# LANGUAGE MultiParamTypeClasses #-}

module Model.UserProfile
    ( UserProfile (..)
    , Name
    , mkUserProfile
    , MachineUsage (..)
    , concretize
    ) where

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
import Model.Machine

type Name = String

newtype UserProfile = UserProfile
    { usages :: M.Map Name MachineUsage }
    deriving (Eq, Show)

data MachineUsage = MachineUsage
    { initially :: State
    , usage     :: [(Second, State)] }
    deriving (Eq, Show)

mkUserProfile :: [(Name, State, [(Second, State)])] -> UserProfile
mkUserProfile = UserProfile . M.fromList .
    map (\ (n, s, us) -> (n, MachineUsage s us))

instance B.Binary MachineUsage where
    put (MachineUsage s us) = do
        B.put s
        B.put us
    get = MachineUsage <$> B.get <*> B.get

instance ToJSON MachineUsage where
    toJSON (MachineUsage s us) = toJSON (s, us)

instance FromJSON MachineUsage where
    parseJSON v = do
        (s, us) <- parseJSON v
        return $ MachineUsage s us

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

concretize :: UserProfile
           -> M.Map Name MachineDescription
           -> [(MachineDescription, MachineUsage)]
concretize (UserProfile uss) mds = M.elems $ M.intersectionWith (,) mds uss
