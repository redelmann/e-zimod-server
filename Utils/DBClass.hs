module Utils.DBClass where 

class DBisable a where
  serialize :: a -> String
  deserialize :: String -> a
