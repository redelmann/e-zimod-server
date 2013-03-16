{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Snap
import Data.Monoid
import Data.Aeson
import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

-- | Launches the HTTP server.
main :: IO ()
main = quickHttpServe site

-- | Main site.
site :: Snap ()
site = route 
    -- Some dumb examples.
    [ ("sum",  sendAsJson =<< sumInt  <$> jsonParam "r") 
    , ("comb", sendAsJson =<< combInt <$> jsonParam "as" <*> jsonParam "bs" )
    , ("plus", sendAsJson =<< plusInt <$> readParam "a" <*> readParam "b") ]
  where
    -- Specifying types.
    sumInt :: [Int] -> Int
    sumInt = sum

    combInt :: [Int] -> [Int] -> [(Int, Int)]
    combInt as bs = (,) <$> as <*> bs

    plusInt :: Int -> Int -> Int
    plusInt = (+)

-- | Responds immediately with specified code and message.
respondWith :: Int -> BS.ByteString -> Snap a
respondWith c b = do
    modifyResponse $ setResponseCode c
    writeBS b
    r <- getResponse
    finishWith r

-- | Requires a parameter, 
--   immediately responding an appropriate message if not present.
requireParam :: BS.ByteString -> Snap BS.ByteString
requireParam param = do
    mReq <- getParam param
    case mReq of
        Just req -> return req
        Nothing  -> respondWith 400 $ "Parameter " <> param <> " not specified."

-- | Reads a parameter,
--   immediately responding an appropriate message if not present or not valid.
readParam :: Read a => BS.ByteString -> Snap a
readParam param = requireParam param >>= getRead
  where
    getRead :: Read a => BS.ByteString -> Snap a
    getRead b = case reads $ BS.unpack b of
        [(v, "")] -> return v
        _         -> respondWith 400 $ "Parameter " <> param <> " not valid."

-- | Reads a parameter encoded via JSON,
--   immediately responding an appropriate message if not present or not valid.
jsonParam :: FromJSON a => BS.ByteString -> Snap a
jsonParam param = requireParam param >>= getFromJson
  where
    getFromJson :: FromJSON a => BS.ByteString -> Snap a
    getFromJson b = do
        case decode $ LBS.fromChunks [b] of
            Just x  -> return x
            Nothing -> respondWith 400 $ "Parameter " <> param <> " not valid."

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
