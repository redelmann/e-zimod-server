
module Model.DBManager
    (resetdb
    ,initconn
    ,addProfile
    ,getProfile
    ) where

import Model.Profile
import Database.HDBC
import Database.HDBC.Sqlite3

-- This module should probably be declared else where ?
-- new directory DB ? or in another utils?
-- first solo try on haskell, be indulgent :p


-- | Drop all the table in the database given in argument
--   and re-create the needed tables
resetdb :: String -> IO () 
resetdb dbname = 
  do
    c <- connectSqlite3 dbname
    currtables <- getTables c
    mapM_ (\t -> run c ("DROP TABLE " ++ t) []) currtables
    mapM_ (\t -> run c ("CREATE TABLE " ++ t) []) tables
    commit c
    where
      tables = ["profiles (id INTEGER PRIMARY KEY, value TEXT)"
               ,"machines (id INTEGER PRIMARY KEY, infos TEXT)"
               ]

-- | open a connection on the given sql database
initconn :: String -> IO Connection
initconn = connectSqlite3
               

-- | Add a profile with the given id
-- TODO: manage exception for non unique id (sql exception from the primary key constraint)
addProfile :: IO Connection -> Integer -> Profile -> IO Integer
addProfile conn id p = 
  do 
    c <- conn
    i <- run c "INSERT INTO profiles VALUES (?,?)" [toSql id, toSql $ serialize p]
    commit c
    return i

-- | recover a Profile from a given id
-- TODO: manage no match
getProfile :: IO Connection -> Integer -> IO Profile 
getProfile conn id = 
  do
    c <- conn
    q <- quickQuery' c "SELECT value FROM profiles where id = ?" [toSql id]
    return $ extract q
    where
      extract = read . fromSql . head . head
