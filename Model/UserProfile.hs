{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module Model.UserProfile
    ( UserProfile (..)
    , Name
    , Id
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
import Control.Monad

import Model.State
import Model.Types
import Model.Machine

type Id = String
type Name = String

data UserProfile = UserProfile
    { userName :: Name
    , usages   :: M.Map Id MachineUsage }
    deriving (Eq, Show)

data MachineUsage = MachineUsage
    { initially :: State
    , usage     :: [(Second, State)] }
    deriving (Eq, Show)

mkUserProfile :: Name -> [(Id, State, [(Second, State)])] -> UserProfile
mkUserProfile name = UserProfile name . M.fromList .
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
    put (UserProfile un as) = do
        B.put un
        B.put as
    get = UserProfile <$> B.get <*> B.get

instance Convertible UserProfile SqlValue where
    safeConvert = Right . SqlByteString . BS.concat . BL.toChunks . B.encode

instance Convertible SqlValue UserProfile where
    safeConvert (SqlByteString bs) = Right $ B.decode $ BL.fromChunks [bs]

instance ToJSON UserProfile where
    toJSON (UserProfile un xs) = object [ "name" .= un, "data" .= xs]

instance FromJSON UserProfile where
    parseJSON (Object v) = UserProfile <$> v .: "name" <*> v .: "data"
    parseJSON _ = mzero

concretize :: UserProfile
           -> M.Map Id MachineDescription
           -> [(MachineDescription, MachineUsage)]
concretize (UserProfile _ uss) mds = M.elems $ M.intersectionWith (,) mds uss
