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
    [ ("getTableProfile",  sendAsJsonP =<< getProfilesH)
    , ("getTableMachine",  sendAsJsonP =<< getMachinesH)
    , ("getTableRelation", sendAsJsonP =<< getRelationH)
    , ("day",              sendAsJsonP =<< getDayH)
    , ("week",             sendAsJsonP =<< getWeekH)
    , ("fridge",           sendAsJsonP =<< getFridgeProfileH)
    , ("randomProfile",    sendAsJsonP =<< randomProfilesH) ]

-- | Day handler.
getDayH :: Snap ([(Int, Joule)], [(Int, Watt)])
getDayH = do
    mms <- pick $ replicateM (4*24) meanAndMax
    let means = zip [1..] $ map (toRational . fst) mms
    let maxs  = zip [1..] $ map (toRational . snd) mms
    return (means, maxs)
  where
    meanAndMax :: Rand (Int, Int)
    meanAndMax = do
        mean <- inRange (20, 50)
        maxv <- (+ mean) <$> inRange (2, 20)
        return (mean * 15 * 60, maxv)

getWeekH :: Snap ([(Int, Joule)], [(Int, Watt)])
getWeekH = do
    mms <- pick $ replicateM 7 meanAndMax
    let means = zip [1..] $ map (toRational . fst) mms
    let maxs  = zip [1..] $ map (toRational . snd) mms
    return (means, maxs)
  where
    meanAndMax :: Rand (Int, Int)
    meanAndMax = do
        mean <- inRange (20, 50)
        maxv <- (+ mean) <$> inRange (2, 20)
        return (mean * 60 * 60 * 24, maxv)

-- | Profiles handler.
getProfilesH :: Snap [(Integer, UserProfile)]
getProfilesH = getTableH "userprofiles"

-- | Machines handler.
getMachinesH :: Snap [(Integer, MachineDescription)]
getMachinesH = getTableH "machines"

-- | General database handler.
getTableH :: (ToJSON a, DBisable a, Eq a) => String -> Snap [(Integer, a)]
getTableH tab = do
  c <- liftIO $ initConn databaseName
  e <- elem tab <$> liftIO (getTables c)
  unless e $ respondWith 404 "table not found"
  liftIO $ getTable c tab

-- | Relation handler.
getRelationH :: Snap [(Integer, Integer)]
getRelationH = liftIO $ do
  c <- initConn databaseName
  getRelation c

-- | Fridge profile handler.
getFridgeProfileH :: Snap Profile
getFridgeProfileH = do
    ts <- jsonParam "times" :: Snap [Double]
    let ts' = sort $ map toRational ts
    let es  = zip ts' $ cycle ["Off", "On"]
    n <- readParam "upto" :: Snap Int
    let Just p = computeProfile fridge "On" es
    return $ p `upTo` toRational n

-- | Computes `n` random profiles.
randomProfilesH :: Snap [Profile]
randomProfilesH = do
    n <- readParam "n"
    pick $ replicateM n prof
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
