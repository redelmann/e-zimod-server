{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.Utils 
    ( (<~>)
    , tests
    ) where

import Test.QuickCheck

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitSuccess, exitFailure)

infixl 0 <~>

data NamedTest = forall t. Testable t => NamedTest String t

(<~>) :: Testable t => String -> t -> NamedTest
(<~>) = NamedTest

exitStatus :: IORef (IO ())
{-# NOINLINE exitStatus #-}
exitStatus = unsafePerformIO (newIORef exitSuccess)

test :: NamedTest -> IO ()
test (NamedTest s t) = do
    r <- quickCheckResult $ whenFail (putStrLn $ "*** " ++ s) $ label s t
    case r of
        (Success _ _ _) -> return ()
        _               -> writeIORef exitStatus exitFailure

tests :: [NamedTest] -> IO ()
tests ts = do
  mapM test ts
  readIORef exitStatus >>= id