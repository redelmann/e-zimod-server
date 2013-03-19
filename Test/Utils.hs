{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.Utils
    ( (<~>)
    , tests
    , withProfile
    ) where

import Test.QuickCheck

import Control.Monad (join)
import Data.IORef
import System.Exit (exitSuccess, exitFailure)

import Model.Types
import Model.Profile

infixl 0 <~>

data NamedTest = forall t . Testable t => NamedTest String t

(<~>) :: Testable t => String -> t -> NamedTest
(<~>) = NamedTest

test :: IORef (IO ()) -> NamedTest -> IO ()
test exitStatus (NamedTest s t) = do
    r <- quickCheckResult $ whenFail (putStrLn $ "*** " ++ s) $ label s t
    case r of
        (Success {}) -> return ()
        _            -> writeIORef exitStatus exitFailure

tests :: [NamedTest] -> IO ()
tests ts = do
    exitStatus <- newIORef exitSuccess
    mapM_ (test exitStatus) ts
    join $ readIORef exitStatus

withProfile :: [(Second, Watt)] -> (Profile -> Bool) -> Bool
withProfile xs f = case mkProfile xs of
    Just p  -> f p
    Nothing -> False
