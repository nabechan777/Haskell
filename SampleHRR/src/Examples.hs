{-# LANGUAGE FlexibleContexts #-}

module Examples (run) where

import Database.Relational.Query.SQLite3
import Account


run :: IO ()
run = do
    conn <- connectSqlite3 "examples.db"
    runImpl conn () sql1

runImpl :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
       => conn -> p -> Relation p a -> IO ()
runImpl conn param rel = do
    putStrLn $ "\nSQL: " ++ show rel ++ "\n"
    records <- runRelation conn rel param
    mapM_ print records
    putStrLn ""

sql1 :: Relation () Account
sql1 = relation $ do
    a <- query account
    wheres $ a ! Account.productCd' `in'` values ["CTK", "SAV", "CD", "MM"]
    return a


-- sql2 :: Relation () Account2
-- sql2 = relation $ do
--     a <- query account
--     desc $ a ! Account.availBalance'
--     return $ Account2 |$| a ! Account.accountId'
--                       |*| a ! Account.productCd'
--                       |*| a ! Account.openDate'
--                       |*| a ! Account.availBalance'
