{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
import Prelude hiding (readFile, putStrLn)
import Data.ByteString.Char8

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
GarmentType
    name String
    deriving Show
|]

connStr :: ConnectionString
connStr = "host=localhost port=5432 database=harry user=harry password=harry" 

main :: IO ()
main = withPostgresqlConn connStr $ runSqlConn $ do
    runMigration migrateAll
    garmentTypeId <- insert $ GarmentType "Trousers"
    putStrLn garmentTypeId
