{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Prelude hiding (catch)
import Snap
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route 
	[ ("sum", withJsonRequest "r" sumHandler) ]

respondWith :: Int -> BS.ByteString -> Snap a
respondWith c b = do
	modifyResponse $ setResponseCode c
	writeBS b
	r <- getResponse
	finishWith r

withJsonRequest :: FromJSON a => BS.ByteString -> (a -> Snap b) -> Snap b
withJsonRequest param handler = do
	mReq <- getParam param
	case mReq of
		Just req -> getFromJson req >>= handler
		Nothing  -> respondWith 400 "No request specified."

getFromJson :: FromJSON a => BS.ByteString -> Snap a
getFromJson b = do
	case decode $ LBS.fromChunks [b] of
		Just x  -> return x
		Nothing -> respondWith 400 "The request is not valid."

sendAsJson :: ToJSON a => a -> Snap b
sendAsJson x = do
	modifyResponse $ setContentType "application/json"
	writeLBS $ encode x
	r <- getResponse
	finishWith r

sumHandler :: [Int] -> Snap ()
sumHandler is = do
	sendAsJson $ sum is : is

