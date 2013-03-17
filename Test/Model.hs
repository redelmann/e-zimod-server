module Main(main) where

import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when)

import Model.Types
import Model.Profile

withProfile :: [(Second, Watt)] -> (Profile -> Bool) -> Bool
withProfile xs f = case mkProfile xs of
    Just p  -> f p
    Nothing -> False

peek1 = withProfile [(0, 10)] $ \ p ->
    peek p 20 == 10 && peek p 42 == 10 && peek p (-20) == 10

peek2 = withProfile [(0, 10), (10, 20)] $ \ p -> all id $
    [ peek p (-20) == 10
    , peek p 0 == 10
    , peek p 5 == 15
    , peek p 2 == 12 
    , peek p 10 == 20 
    , peek p 42 == 20 ]

test :: Bool -> IO ()
test t = when (not $ t) exitFailure

main :: IO ()
main = do
    test peek1
    test peek2
    exitSuccess
