{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.Utils 
    ( (<~>)
    , tests
    , withProfile
    ) where

import Test.QuickCheck

import Data.Maybe (fromJust)
import Data.IORef
import System.Exit (exitSuccess, exitFailure)

import Model.Types
import Model.Profile

infixl 0 <~>

data NamedTest = forall t. Testable t => NamedTest String t

(<~>) :: Testable t => String -> t -> NamedTest
(<~>) = NamedTest

test :: IORef (IO ()) -> NamedTest -> IO ()
test exitStatus (NamedTest s t) = do
    r <- quickCheckResult $ whenFail (putStrLn $ "*** " ++ s) $ label s t
    case r of
        (Success _ _ _) -> return ()
        _               -> writeIORef exitStatus exitFailure

tests :: [NamedTest] -> IO ()
tests ts = do
    exitStatus <- newIORef exitSuccess
    mapM (test exitStatus) ts
    readIORef exitStatus >>= id

withProfile :: [(Second, Watt)] -> (Profile -> Bool) -> Bool
withProfile xs f = case mkProfile xs of
    Just p  -> f p
    Nothing -> False