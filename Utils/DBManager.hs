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
    commit c
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
  -- mkUserProfile :: Name -> [(Name, State, [(Second, State)])] -> UserProfile
  addProfile c 0 (mkUserProfile "Marc"  [("frigo", "off",
    [(10, "on"), (50, "off"), (220, "on")])])
  addProfile c 1 (mkUserProfile "Paul" [("frigo", "off",
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
  addMachine c (MachineDescription "TVy, CRT" [("on", Repeat $ fromJust $ mkProfile [(0,200)]),
                                              ("off", once $ fromJust $ mkProfile [(0,5)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Personal computerish" [("on", Repeat $ fromJust $ mkProfile [(0,250]),
                                              ("off", Once $ fromJust $ mkProfile [(0,21)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Heating, central furnace" [("on", Repeat $ fromJust $ mkProfile [(0,340]),
                                              ("off", Once $ fromJust $ mkProfile [(0,4)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Cooling" [("on", Repeat $ fromJust $ mkProfile [(0,600]),
                                              ("off", Once $ fromJust $ mkProfile [(0,3)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "36inch ceiling fan" [("on", Repeat $ fromJust $ mkProfile [(0,55]),
                                              ("off", Once $ fromJust $ mkProfile [(0,1)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Clothes dryer (electric)" [("on", Repeat $ fromJust $ mkProfile [(0,4400]),
                                              ("off", Once $ fromJust $ mkProfile [(0,21)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Clothes washer" [("on", Repeat $ fromJust $ mkProfile [(0,400]),
                                              ("off", Once $ fromJust $ mkProfile [(0,21)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Dishwasher" [("on", Repeat $ fromJust $ mkProfile [(0,3000]),
                                              ("off", Once $ fromJust $ mkProfile [(0,21)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Microwave oven" [("on", Repeat $ fromJust $ mkProfile [(0,1440]),
                                              ("off", Once $ fromJust $ mkProfile [(0,5)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Game Console: PS3" [("on", Repeat $ fromJust $ mkProfile [(0,194]),
                                              ("off", Once $ fromJust $ mkProfile [(0,10)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Game Console: PS2" [("on", Repeat $ fromJust $ mkProfile [(0,30]),
                                              ("off", Once $ fromJust $ mkProfile [(0,10)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Coffee maker" [("on", Repeat $ fromJust $ mkProfile [(0,900]),
                                              ("off", Once $ fromJust $ mkProfile [(0,1)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Clock radio" [("on", Repeat $ fromJust $ mkProfile [(0,4]),
                                              ("off", Once $ fromJust $ mkProfile [(0,1)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Vacuum cleaner" [("on", Repeat $ fromJust $ mkProfile [(0,1300]),
                                              ("off", Once $ fromJust $ mkProfile [(0,0)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "DVD Player" [("on", Repeat $ fromJust $ mkProfile [(0,10]),
                                              ("off", Once $ fromJust $ mkProfile [(0,8)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Amplifier (music)" [("on", Repeat $ fromJust $ mkProfile [(0,40]),
                                              ("off", Once $ fromJust $ mkProfile [(0,30)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Printer (Laser)" [("on", Repeat $ fromJust $ mkProfile [(0,130]),
                                              ("off", Once $ fromJust $ mkProfile [(0,2)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Printer (Inkjet)" [("on", Repeat $ fromJust $ mkProfile [(0,5]),
                                              ("off", Once $ fromJust $ mkProfile [(0,2)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Phone, cordless with answering machine" [("on", Repeat $ fromJust $ mkProfile [(0,5]),
                                              ("off", Once $ fromJust $ mkProfile [(0,3)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Modem, DSL" [("on", Repeat $ fromJust $ mkProfile [(0,5]),
                                              ("off", Once $ fromJust $ mkProfile [(0,1)])]
                                             [("on","off",5,10),("off","on",5,20)])
  addMachine c (MachineDescription "Fax (Laser)" [("on", Repeat $ fromJust $ mkProfile [(0,6]),
                                              ("off", Once $ fromJust $ mkProfile [(0,6)])]
                                             [("on","off",5,10),("off","on",5,20)])

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

-- | Adds a Profile with the given id.
addProfile :: Connection -> Integer -> UserProfile -> IO Bool
addProfile c = addInto c "userprofiles"

-- | Add a Machine with the given id
addMachine :: Connection -> Integer -> MachineDescription -> IO Bool
addMachine c = addInto c "machines"

-- | Recovers a Profile from a given id.
getProfile :: Connection -> Integer -> IO UserProfile
getProfile c i = fromJust <$> getByID c "userprofiles" i
