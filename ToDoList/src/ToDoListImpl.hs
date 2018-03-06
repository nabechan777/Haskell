module ToDoListImpl
    ( dispatch
    ) where

import Control.Exception
import Data.List
import System.Directory
import System.IO

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "bump" = bump
dispatch "remove" = remove
dispatch "view" = view
dispatch command = doesntExist command

doesntExist :: String -> [String] -> IO ()
doesntExist command _ = putStrLn $ "The " ++ command ++ " command doesn't exist."

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "The add command takes exactly two arguments.(ToDoFileName and Item enclosed in \" \")"

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let
        todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn $ unlines numberedTasks
view _ = putStrLn "The view command takes exactly one argument.(ToDoFileName)"

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let
        todoTasks = lines contents
        number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    putStrLn "Remove your TO-DO items:"
    mapM_ putStrLn $ zipWith (\n item -> show n ++ " - " ++ item) [0..] newTodoItems
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle $ unlines newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
remove _ = putStrLn "The remove command takes exactly two arguments.(ToDoFileName and ItemNumber)"

bump :: [String] -> IO ()
bump [fileName, numberString] = do
    contents <- readFile fileName
    let
        todoTasks = lines contents
        number = read numberString
        tempItem = todoTasks !! number
        newTodoItems = (tempItem :) $ delete tempItem todoTasks
    putStrLn "The selected item is moved to head: "
    mapM_ putStrLn $ zipWith (\n item -> show n ++ " - " ++ item) [0..] newTodoItems
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle $ unlines newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
bump _ = putStrLn "The bump command takes two arguments.(ToDoFileName and ItemNumber)"
