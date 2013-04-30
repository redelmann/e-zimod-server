{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}

-- | This modules defines creation and manipulation of machines.
module Model.Machine (
    -- * Machine description
      MachineDescription (..)
    , State
    -- * Computations
    , computeProfile
    -- * Cyclic behavior
    , Cyclic (..)
    , uncycle
    ) where

import Database.HDBC
import Data.Convertible.Base
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as B
import Control.Monad (mzero)
import Control.Applicative
import Control.Arrow (first)
import Data.List (find, foldl', isPrefixOf)
import Data.Aeson

import Model.Types
import Model.Profile

-- | Type of possible states of machines.
type State = String

-- | Describes a machine.
data MachineDescription = MachineDescription
    { name        :: String
    -- ^ Name of the machine
    , behavior    :: [(State, Cyclic Profile)]
    -- ^ Behavior in various states, as described by possibly cyclic profiles
    , transitions :: [(State, State, Second, Second)]
    -- ^ Transitions time between states, along with possible postpone time
    } deriving (Show, Eq)

instance B.Binary MachineDescription where
    put (MachineDescription n b ts) = do
        B.put n
        B.put b
        B.put ts

    get = MachineDescription <$> B.get <*> B.get <*> B.get

instance Convertible MachineDescription SqlValue where
    safeConvert = Right . SqlByteString . BS.concat . BL.toChunks . B.encode

instance Convertible SqlValue MachineDescription where
    safeConvert (SqlByteString bs) = Right $ B.decode $ BL.fromChunks [bs]

instance FromJSON MachineDescription where
    parseJSON (Object v) = MachineDescription
        <$> v .: "name"
        <*> v .: "behavior"
        <*> v .: "transitions"
    parseJSON _ = mzero

instance ToJSON MachineDescription where
    toJSON (MachineDescription n b t) =
        object ["name" .= n, "behavior" .= b, "transitions" .= t]

{- | Given a machine, an initial state and
     a list of states to transition to, sorted by increasing time,
     computes the resulting power profile. -}
computeProfile :: MachineDescription  -- ^ The machine
               -> State               -- ^ The initial state of the machine
               -> [(Second, State)]   -- ^ List of states to transition to
               -> Maybe Profile
computeProfile md is xs = do
    ip <- uncycle <$> lookup is (behavior md)
    (clean . foldr1 andThen . reverse . snd) <$> foldl' f (Just (is, [ip])) xs
  where
    f :: Maybe (State, [Profile])
      -> (Second, State)
      -> Maybe (State, [Profile])
    f Nothing _ = Nothing
    f (Just (s1, p : ps)) (t, s2) = do
        -- Trimming the last computed profile upto time t.
        let pa = p `upTo` t
        -- Getting transition time.
        (_, _, dt, _) <- find (\ (a, b, _, _) -> a == s1 && b == s2) (transitions md)
        -- Getting behavior in new state.
        cp <- lookup s2 (behavior md)
        -- Uncycling the behavior.
        let pb = uncycle cp
        -- Returning the new states along with the new profiles.
        return (s2, pb `deferredBy` (t + dt) : pa : ps)

{- | Represents possibly cyclic elements.

     This is used to allow serializing
     infinite cyclic structures as finite ones. -}
data Cyclic a = Repeat a  -- ^ The element is repeated ad infinitum
              | Once a    -- ^ The element is not repeated
              deriving (Show, Eq)

instance (B.Binary a) => B.Binary (Cyclic a) where
    put (Repeat x) = do
        B.putWord8 1
        B.put x
    put (Once x) = do
        B.putWord8 0
        B.put x

    get = do
        w <- B.getWord8
        x <- B.get
        return $ case w of
            0 -> Once x
            1 -> Repeat x

instance (B.Binary a) => Convertible (Cyclic a) SqlValue where
    safeConvert = Right . SqlByteString . BS.concat . BL.toChunks . B.encode

instance (B.Binary a) => Convertible SqlValue (Cyclic a) where
    safeConvert (SqlByteString bs) = Right $ B.decode $ BL.fromChunks [bs]

instance ToJSON a => ToJSON (Cyclic a) where
    toJSON (Once x)   = object ["cyclic" .= False, "data" .= x]
    toJSON (Repeat x) = object ["cyclic" .= True,  "data" .= x]

instance FromJSON a => FromJSON (Cyclic a) where
    parseJSON (Object v) = do
        c <- v .: "cyclic"
        d <- v .: "data"
        return $ if c then Once d else Repeat d
    parseJSON _ = mzero

-- | Computes the infinite profile corresponding to this cyclic profile.
uncycle :: Cyclic Profile -> Profile
uncycle (Once p) = p
uncycle (Repeat p) = p'
  where
    xs = getList p
    t = fst $ last xs
    p' = p `andThen` (p' `deferredBy` t)

instance Functor Cyclic where
    fmap f (Once x) = Once (f x)
    fmap f (Repeat x) = Repeat (f x)
