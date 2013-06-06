{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Utils.DBManager
    ( resetDB
    , initConn
    , addMachine
    , deleteMachine
    , lastInsertedId
    , withDataBase
    , addProfile
    , deleteProfile
    , getProfile
    , getByID
    , getTable
    , getRelation
    , DBisable
    , module Database.HDBC
    , module Database.HDBC.Sqlite3
    , tempfilldb
    ) where

import Control.Monad (void)
import Data.Functor ((<$>))
import Data.Maybe (listToMaybe, fromJust)
import Data.Convertible.Base
import Database.HDBC
import Database.HDBC.Sqlite3

import Model.Profile
import Model.UserProfile
import Model.Machine
import Data.Aeson
import Settings

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
    commit c
  where
    tables = [ "userprofiles (id INTEGER PRIMARY KEY AUTOINCREMENT, value TEXT)"
             , "machines (id INTEGER PRIMARY KEY AUTOINCREMENT, value TEXT)"
             , "relations (pid INTEGER, mid INTEGER" ++
                          ", FOREIGN KEY(pid) REFERENCES userprofiles(id)" ++
                          ", FOREIGN KEY(mid) REFERENCES machines(id)" ++
                          ", PRIMARY KEY(mid, pid))"]

tempfilldb :: IO ()
tempfilldb = do
  c <- initConn "test.db"
  -- mkUserProfile :: Name -> [(Name, State, [(Second, State)])] -> UserProfile
  addProfile c (mkUserProfile "Marc"  [("frigo", "off",
    [(10, "on"), (50, "off"), (220, "on")])])
  addProfile c (mkUserProfile "Paul" [("frigo", "off",
    [(10, "on"), (50, "off"), (220, "on")]),
    ("radiateur", "on",
    [(200, "off"), (400, "on")])])
  -- MachineDescription 
  -- Name :: String
  -- beahvior :: [(State, Cyclic Profile)] 
  -- transition :: [(State, State, Second, Second)]
  addMachine c (MachineDescription "fridgy" [("on", Repeat $ fromJust $ mkProfile [(0,0),(10,200),(70,150),(72,0)]),
                                            ("off", Once $ fromJust $ mkProfile [(0,10),(10,10)])]
                                            [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "lampy" [("on", Repeat $ fromJust $ mkProfile [(0,50),(10,50)]),
                                           ("off", Once $ fromJust $ mkProfile [(0,0),(10,0)])]
                                           [("on","off",5,10),("off","on",5,20)])
  commit c
  disconnect c

-- | Opens a connection on the given sql database.
initConn :: String -> IO Connection
initConn = connectSqlite3

addInto :: DBisable a => Connection -> String -> a -> IO Bool
addInto c table input = do
    n <- run c ("INSERT INTO " ++ table ++ " VALUES ?")
           [toSql input]
    return (n > 0)

getByID :: DBisable a => Connection -> String -> Integer -> IO (Maybe a)
getByID c table i = do
    q <- quickQuery' c ("SELECT value FROM " ++ table ++ " WHERE id=?")
      [toSql i]
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

-- | Adds a Profile.
addProfile :: Connection -> UserProfile -> IO Bool
addProfile c = addInto c "userprofiles"

-- | Add a Machine.
addMachine :: Connection -> MachineDescription -> IO Bool
addMachine c = addInto c "machines"

deleteMachine :: Connection -> Integer -> IO ()
deleteMachine c i = void $ quickQuery' c ("DELETE FROM machines WHERE id = ?") [toSql i]

deleteProfile :: Connection -> Integer -> IO ()
deleteProfile c i = void $ quickQuery' c ("DELETE FROM profiles WHERE id = ?") [toSql i]

-- | Recovers a Profile from a given id.
getProfile :: Connection -> Integer -> IO UserProfile
getProfile c i = fromJust <$> getByID c "userprofiles" i

lastInsertedId :: Connection -> String -> IO Integer
lastInsertedId c table = do
    extract <$> quickQuery' c ("SELECT last_insert_rowid() FROM " ++ table) []
  where
    extract :: [[SqlValue]] -> Integer
    extract [[v]] = fromSql v
