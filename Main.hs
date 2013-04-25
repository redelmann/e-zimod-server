{-# LANGUAGE OverloadedStrings, ConstraintKinds, FlexibleContexts #-}

module Main (main) where

import Snap
import Data.Monoid
import Data.Aeson
import Data.List (sort)
import Control.Applicative
import Control.Monad
import Control.Monad.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Profiles
import Model
import Settings
import Utils.DBManager

instance RandPicker Snap where
    pick = liftIO . pick

-- | Launches the HTTP server.
main :: IO ()
main = quickHttpServe site

-- | Main site.
site :: Snap ()
site = route
    -- Some small examples.
    [ ("peek",  sendAsJson =<< peekl
        <$> jsonParam "profile"
        <*> (doubleToRational <$> readParam "second"))
    , ("energy", sendAsJson =<< computeEnergy
        <$> (doubleToRational <$> readParam "a")
        <*> (doubleToRational <$> readParam "b")
        <*> jsonParam "profile" )
    , ("randomProfile", sendAsJsonP =<< randomProfiles =<< readParam "n")
    , ("getTableProfile",sendAsJsonP =<< (getTableH "profiles" :: Snap [(Integer, Cyclic Profile)]))
    , ("fridge", sendAsJsonP =<< getFridgeProfile) ]
  where
    doubleToRational :: Double -> Rational
    doubleToRational = toRational

getTableH :: (ToJSON a, DBisable a, Eq a) => String -> Snap [(Integer, a)]
getTableH tab = do
  c <- liftIO $ initConn databaseName 
  e <- elem tab <$> liftIO (getTables c)
  unless e $ respondWith 404 "table not found" 
  liftIO $ getTable c tab

getFridgeProfile :: Snap Profile
getFridgeProfile = do
    ts <- jsonParam "times" :: Snap [Double]
    let ts' = sort $ map toRational ts
    let es  = zip ts' $ cycle ["Off", "On"]
    n <- readParam "upto" :: Snap Int
    let Just p = computeProfile fridge "On" es
    return $ p `upTo` toRational n

-- | Computes `n` random profiles.
randomProfiles :: Int -> Snap [Profile]
randomProfiles n = pick $ replicateM n prof
  where
    prof :: Rand Profile
    prof = do
        a <- ratInRange (0, 50)
        b <- ratInRange (0, 50)
        i <- inRange (2, 4)
        pts <- replicateM i pt
        let Just p = mkProfile ((0, a) : (3600, b) : pts)
        return p

    pt :: Rand (Second, Watt)
    pt = (,) <$> ratInRange (0, 3600) <*> ratInRange (0, 50)

    ratInRange :: (Int, Int) -> Rand Rational
    ratInRange r = toRational <$> inRange r

-- | Responds immediately with specified code and message.
respondWith :: Int -> BS.ByteString -> Snap a
respondWith c b = do
    modifyResponse $ setResponseCode c
    writeBS b
    r <- getResponse
    finishWith r

{- | Requires a parameter,
     immediately responding an appropriate message if not present. -}
requireParam :: BS.ByteString -> Snap BS.ByteString
requireParam param = do
    mReq <- getParam param
    case mReq of
        Just req -> return req
        Nothing  -> respondWith 400 $ "Parameter " <> param <> " not specified."

{- | Reads a parameter, immediately responding
     an appropriate message if not present or not valid. -}
readParam :: Read a => BS.ByteString -> Snap a
readParam param = requireParam param >>= getRead
  where
    getRead :: Read a => BS.ByteString -> Snap a
    getRead b = case reads $ BS.unpack b of
        [(v, "")] -> return v
        _         -> respondWith 400 $ "Parameter " <> param <> " not valid."

{- | Reads a parameter encoded via JSON, immediately responding
     an appropriate message if not present or not valid. -}
jsonParam :: FromJSON a => BS.ByteString -> Snap a
jsonParam param = requireParam param >>= getFromJson
  where
    getFromJson :: FromJSON a => BS.ByteString -> Snap a
    getFromJson b = case decode $ LBS.fromChunks [b] of
        Just x  -> return x
        Nothing -> respondWith 400 $ "Parameter " <> param <> " not valid."

-- | Sends immediately a value, encoded in JSONP.
sendAsJsonP :: ToJSON a => a -> Snap b
sendAsJsonP x = do
    cb <- requireParam "callback"
    modifyResponse $ setContentType "application/javascript"
    writeBS $ cb <> "("
    writeLBS $ encode x
    writeBS ")"
    r <- getResponse
    finishWith r

-- | Sends immediately a value, encoded in JSON.
sendAsJson :: ToJSON a => a -> Snap b
sendAsJson x = do
    modifyResponse $ setContentType "application/json"
    writeLBS $ encode x
    r <- getResponse
    finishWith r

-- | Sends immediately a value, as plain text.
sendAsText :: Show a => a -> Snap b
sendAsText x = do
    modifyResponse $ setContentType "plain/text"
    writeBS $ BS.pack $ show x
    r <- getResponse
    finishWith r
