{-# LANGUAGE OverloadedStrings, ConstraintKinds, FlexibleContexts #-}

module Main (main) where

import Prelude hiding (lookup)
import Snap hiding (forM)
import Data.Monoid
import Data.Aeson
import Data.Traversable (forM)
import Data.List (sort, foldl1')
import Data.Map (keys, assocs, lookup, fromList)
import Data.Maybe
import Control.Exception
import Control.Applicative
import Control.Monad hiding (forM)
import Control.Monad.Random
import qualified Control.Monad.CatchIO as CIO
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Profiles
import Model
import Settings
import Simulation
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
    
    -- Machine management.
    , ("addMachine",       sendAsJsonP =<< addMachineH)
    , ("deleteMachine",    sendAsJsonP =<< deleteMachineH)

    -- User profile management.
    , ("addUserProfile",       sendAsJsonP =<< addUserProfileH)
    , ("deleteUserProfile",    sendAsJsonP =<< deleteUserProfileH)

    , ("quarter", sendAsJsonP =<< getQuarterH)
    , ("day",     sendAsJsonP =<< getDayH)
    , ("week",    sendAsJsonP =<< getWeekH)]


error400onException :: Snap a -> Snap a
error400onException action = action `CIO.catch` handler
  where
    handler :: SomeException -> Snap a
    handler _ = respondWith 400 "Bad request"

-- | Machine adder handler.
addMachineH :: Snap Integer
addMachineH = do
    md <- jsonParam "machine"
    withConnection $ \ c -> do
        addMachine c md
        lastInsertedId c "machines"

-- | Machine delete handler.
deleteMachineH :: Snap ()
deleteMachineH = do
    i <- readParam "id"
    withConnection $ \ c ->
        deleteMachine c i

-- | User profile adder handler.
addUserProfileH :: Snap Integer
addUserProfileH = do
    pr <- jsonParam "profile"
    withConnection $ \ c -> do
        addProfile c pr
        lastInsertedId c "userprofiles"

-- | User profile delete handler.
deleteUserProfileH :: Snap ()
deleteUserProfileH = do
    i <- readParam "id"
    withConnection $ \ c ->
        deleteProfile c i

getQuarterH :: Snap [Profile]
getQuarterH = do
    paramBuilder <- getParameters
    from <- toRational <$> (readParam "from" :: Snap Integer)
    let params = paramBuilder (from + 900)
    forM (assocs $ machines params) $ \ (mid, md) -> do
        let Just machineUsage = lookup mid (usages $ userProfile params)
        let Just profile = computeProfile md (initially machineUsage) (usage machineUsage)
        return $ profile `fromTime` from `upTo` 900

getParameters :: Snap (Second -> Parameter)
getParameters = do
    userProfileId <- readParam "id"
    (machines, userProfile) <- withConnection $ \ c -> do
        Just userProfile <- getByID c "userprofiles" userProfileId :: IO (Maybe UserProfile)
        machines <- forM (keys $ usages userProfile) $ \ machineId -> do
            Just m <- getByID c "machines" (read machineId) :: IO (Maybe MachineDescription)
            return (machineId, m)
        return (fromList machines, userProfile)
    return $ Parameter userProfile machines 900

-- | Day handler.
getDayH :: Snap ([(Int, Joule)], [(Int, Watt)])
getDayH = do
    paramBuilder <- getParameters
    from <- toRational <$> (readParam "from" :: Snap Integer)
    let params = paramBuilder (from + totalTime)
    ps <- forM (assocs $ machines params) $ \ (mid, md) -> do
        let Just machineUsage = lookup mid (usages $ userProfile params)
        let Just profile = computeProfile md (initially machineUsage) (usage machineUsage)
        return $ profile `fromTime` from `upTo` totalTime
    let spl = sampledValues $ foldl1' merge $ map (takeOnly (24 * 4) . computeSample sampleTime) ps
    let mxs = take (24 * 4) $ computeMaximums sampleTime $ foldl1' combine ps
    return (zip [0..] spl, zip [0..] mxs)
  where
    sampleTime = 900
    totalTime = 60 * 60 * 24

getWeekH :: Snap ([(Int, Joule)], [(Int, Joule)])
getWeekH = do
    paramBuilder <- getParameters
    from <- toRational <$> (readParam "from" :: Snap Integer)
    let params = paramBuilder (from + totalTime)
    ps <- forM (assocs $ machines params) $ \ (mid, md) -> do
        let Just machineUsage = lookup mid (usages $ userProfile params)
        let Just profile = computeProfile md (initially machineUsage) (usage machineUsage)
        return $ profile `fromTime` from `upTo` totalTime
    let spl = sampledValues $ foldl1' merge $ map (takeOnly 7 . computeSample sampleTime) ps
    let mxs = map maximum $ splits $ sampledValues $
              foldl1' merge $ map (takeOnly (24 * 4 * 7) . computeSample quarterTime) ps
    return (zip [0..] spl, zip [0..] mxs)
  where
    quarterTime = 900
    sampleTime = 60 * 60 * 24
    totalTime = 60 * 60 * 24 * 7

    splits :: [a] -> [[a]]
    splits [] = []
    splits xs = let (as, bs) = splitAt (24 * 4) xs in as : splits bs

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
jsonParam param = do
    requireParam param >>= (liftIO . BS.putStrLn)
    readParam param >>= getFromJson
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

-- | Excutes an action on the database.
withConnection :: (Connection -> IO a) -> Snap a
withConnection = liftIO . withDataBase databaseName
