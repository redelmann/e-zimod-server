{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Utils.DBManager
    ( resetDB
    , initConn
    , addProfile
    , getProfile
    , getTable
    , DBisable
    , module Database.HDBC
    , module Database.HDBC.Sqlite3
    ) where

import Data.Convertible.Base
import Database.HDBC
import Database.HDBC.Sqlite3

import Model.Profile
import Model.Machine
import Data.Aeson
import Control.Arrow

type DBisable a = (Convertible a SqlValue, Convertible SqlValue a)

-- | Excutes an action on a given database.
withDataBase :: String -> (Connection -> IO a) -> IO a
withDataBase dbname action = do
    c <- initConn dbname
    a <- withTransaction c action
    disconnect c
    return a

{- | Drops all the table in the database given in argument
     and re-creates the needed tables. -}
resetDB :: Connection -> IO ()
resetDB c = do
    currtables <- getTables c
    mapM_ (\ t -> run c ("DROP TABLE " ++ t) []) currtables
    mapM_ (\ t -> run c ("CREATE TABLE " ++ t) []) tables
  where
    tables = [ "profiles (id INTEGER PRIMARY KEY, value TEXT)"
             , "machines (id INTEGER PRIMARY KEY, value TEXT)"
             , "relations (pid INTEGER, mid INTEGER, state TEXT" ++
                          ", FOREIGN KEY(pid) REFERENCES profiles(id)" ++
                          ", FOREIGN KEY(mid) REFERENCES machines(id)" ++
                          ", PRIMARY KEY(mid, pid))"]

-- | Opens a connection on the given sql database.
initConn :: String -> IO Connection
initConn = connectSqlite3

addInto :: DBisable a => Connection -> String -> Integer -> a -> IO Bool
addInto c table i input = do
    n <- run c ("INSERT INTO " ++ table ++ " VALUES (?, ?)")
           [toSql i, toSql input]
    return (n > 0)

getTable :: (DBisable a, ToJSON a) => Connection -> String -> IO [(Integer, a)]
getTable c table = do
    q <- quickQuery' c ("SELECT * FROM " ++ table) []
    return $ map extract q
    where
      extract :: (DBisable a, ToJSON a) => [SqlValue] -> (Integer,a)
      extract [i,v] = (fromSql i, fromSql v)

getForm :: DBisable a => Connection -> String -> Integer -> IO a
getForm c table i = do
    q <- quickQuery' c ("SELECT value FROM " ++ table ++  " where id = ?")
           [toSql i]
    return $ extract q
    where
      extract :: DBisable a => [[SqlValue]] -> a
      extract [[v]] = fromSql v

-- | Adds a Profile with the given id.
addProfile :: Connection -> Integer -> Cyclic Profile -> IO Bool
addProfile c = addInto c "profiles"

-- | Recovers a Profile from a given id.
getProfile :: Connection -> Integer -> IO (Cyclic Profile)
getProfile c i = do
    q <- quickQuery c "SELECT value FROM profiles where id = ?" [toSql i]
    return $ extract q
    where
      extract :: [[SqlValue]] -> Cyclic Profile
      extract [[v]] = fromSql v
