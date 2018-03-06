{-# LANGUAGE TemplateHaskell #-}

module Database.Record.TH.SQLite3
    ( defineTable
    ) where

import GHC.Generics (Generic)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Language.Haskell.TH (Q, Dec)

defineTable :: FilePath -> String -> Q [Dec]
defineTable fileName tableName = defineTableFromDB catalog driver schemaName tableName derive
    where
        catalog = connectSqlite3 fileName
        driver = driverSQLite3 { typeMap = [("FLOAT", [t|Double|]), ("INTEGER", [t|Int|])] }
        schemaName = "main"
        derive = [''Show, ''Generic]
