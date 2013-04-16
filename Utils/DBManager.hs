module Utils.DBManager
    ( resetDB
    , initConn
    , addProfile
    , getProfile
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3

import Model.Profile
import Utils.DBClass
import Model.Machine

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
             , "machines (id INTEGER PRIMARY KEY, name TEXT, type TEXT)"
             , "relations (pid INTEGER, mid INTEGER, state TEXT" ++
                          ", FOREIGN KEY(pid) REFERENCES profiles(id)" ++
                          ", FOREIGN KEY(mid) REFERENCES machines(id)" ++
                          ", PRIMARY KEY(mid,pid))"]

-- | Opens a connection on the given sql database.
initConn :: String -> IO Connection
initConn = connectSqlite3

addInto :: DBisable a => Connection -> String -> Integer -> a -> IO Bool
addInto c table i input = do
    n <- run c ("INSERT INTO " ++ table ++ " VALUES (?, ?)")
           [toSql i, toSql $ serialize input]
    return (n > 0)

getForm :: DBisable a => Connection -> String -> Integer -> IO a
getForm c table i = do
    q <- quickQuery c ("SELECT value FROM " ++ table ++  " where id = ?")
           [toSql i]
    return $ extract q
    where
      extract :: DBisable a => [[SqlValue]] -> a
      extract [[v]] = deserialize $ fromSql v

-- | Adds a Profile with the given id.
addProfile :: Connection -> Integer -> Cyclic Profile -> IO Bool
addProfile c = addInto c "profiles"

-- | Recovers a Profile from a given id.
getProfile :: Connection -> Integer -> IO (Maybe (Cyclic Profile))
getProfile c i = do
    q <- quickQuery c "SELECT value FROM profiles where id = ?" [toSql i]
    return $ extract q
    where
      extract :: [[SqlValue]] -> Maybe (Cyclic Profile)
      extract [[v]] = case reads $ fromSql v of
        [(cp, "")] -> Just cp
        _         -> Nothing
      extract _ = Nothing
