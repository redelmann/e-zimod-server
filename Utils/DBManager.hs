module Utils.DBManager
    ( resetdb
    , initconn
    , addProfile
    , getProfile
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3

import Model.Profile

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
             , "machines (id INTEGER PRIMARY KEY, infos TEXT)" ]

-- | Opens a connection on the given sql database.
initConn :: String -> IO Connection
initConn = connectSqlite3


-- | Adds a Profile with the given id.
addProfile :: Connection -> Integer -> Profile -> IO Bool
addProfile c i p = do
    n <- run c "INSERT INTO profiles VALUES (?, ?)"
        [toSql i, toSql $ serialize p]
    return (n > 0)

-- | Recovers a Profile from a given id.
getProfile :: Connection -> Integer -> IO (Maybe Profile)
getProfile c i = do
    q <- quickQuery c "SELECT value FROM profiles where id = ?" [toSql i]
    return $ extract q
    where
      extract :: [[SqlValue]] -> Maybe Profile
      extract [[v]] = case reads $ fromSql v of
        [(p, "")] -> Just p
        _         -> Nothing
      extract _ = Nothing
