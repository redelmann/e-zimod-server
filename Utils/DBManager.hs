{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Utils.DBManager
    ( resetDB
    , initConn
    , addProfile
    , getProfile
    , getByID
    , getTable
    , getRelation
    , DBisable
    , module Database.HDBC
    , module Database.HDBC.Sqlite3
    , tempfilldb
    ) where

import Data.Functor ((<$>))
import Data.Maybe (listToMaybe, fromJust)
import Data.Convertible.Base
import Database.HDBC
import Database.HDBC.Sqlite3

import Model.Profile
import Model.UserProfile
import Model.Machine
import Data.Aeson

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
    tables = [ "userprofiles (id INTEGER PRIMARY KEY, value TEXT)"
             , "machines (id INTEGER PRIMARY KEY, value TEXT)"
             , "relations (pid INTEGER, mid INTEGER" ++
                          ", FOREIGN KEY(pid) REFERENCES userprofiles(id)" ++
                          ", FOREIGN KEY(mid) REFERENCES machines(id)" ++
                          ", PRIMARY KEY(mid, pid))"]

tempfilldb :: IO ()
tempfilldb = do
  c <- initConn "test.db"
  --addProfile c 0 (Once $ square 0 0 10 5)
  --addProfile c 1 (Repeat $ square 0 5 10 8)
  --addProfile c 2 (Once $ constant 5)
  commit c
  disconnect c

-- | Opens a connection on the given sql database.
initConn :: String -> IO Connection
initConn = connectSqlite3

addInto :: DBisable a => Connection -> String -> Integer -> a -> IO Bool
addInto c table i input = do
    n <- run c ("INSERT INTO " ++ table ++ " VALUES (?, ?)")
           [toSql i, toSql input]
    return (n > 0)

getByID :: DBisable a => Connection -> String -> Integer -> IO (Maybe a)
getByID c table i = do
    q <- quickQuery' c ("SELECT value FROM " ++ table ++ " WHERE id=?") [toSql i]
    return $ fmap extract $ listToMaybe q
  where
    extract :: DBisable a => [SqlValue] -> a
    extract [v] = fromSql v

getTable :: DBisable a => Connection -> String -> IO [(Integer, a)]
getTable c table = do
    q <- quickQuery' c ("SELECT * FROM " ++ table) []
    return $ map extract q
  where
    extract :: DBisable a => [SqlValue] -> (Integer, a)
    extract [i, v] = (fromSql i, fromSql v)


getRelation :: Connection -> IO [(Integer, Integer)]
getRelation c = do
    q <- quickQuery' c "SELECT * FROM relations" []
    return $ map extract q
  where
    extract :: [SqlValue] -> (Integer, Integer)
    extract [i, j] = (fromSql i, fromSql j)

getForm :: DBisable a => Connection -> String -> Integer -> IO a
getForm c table i = do
    q <- quickQuery' c ("SELECT value FROM " ++ table ++  " where id = ?")
           [toSql i]
    return $ extract q
  where
    extract :: DBisable a => [[SqlValue]] -> a
    extract [[v]] = fromSql v

-- | Adds a Profile with the given id.
addProfile :: Connection -> Integer -> UserProfile-> IO Bool
addProfile c = addInto c "userprofiles"

-- | Recovers a Profile from a given id.
getProfile :: Connection -> Integer -> IO UserProfile
getProfile c i = fromJust <$> getByID c "userprofiles" i

