module ReaderImpl where

import Data.Map as Map
import Data.Maybe
import Control.Monad.Reader

type Bindings = Map String Int


run :: IO ()
run = do
    let sample = Map.fromList [("count", 3), ("1", 1), ("b", 2)]
    putStr $ "Count is correct for bindings: " ++ (show sample) ++ ": "
    putStrLn $ show (isCountCorrect sample)
    let s = "12345"
        modifiedLen = runReader modifiedContentLen s
        len = runReader contentLen s
    putStrLn $ "Modified 's' length: " ++ (show modifiedLen)
    putStrLn $ "Original 's' length: " ++ (show len)
    runReaderT printReaderContent "Some Content"

isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = runReader isCountCorrectImpl bindings
    where
        isCountCorrectImpl :: Reader Bindings Bool
        isCountCorrectImpl = do
            count <- asks (lookupVar "count")
            bindings <- ask
            return (count == (Map.size bindings))

        lookupVar :: String -> Bindings -> Int
        lookupVar name bindings = fromJust (Map.lookup name bindings)

contentLen :: Reader String Int
contentLen = do
    content <- ask
    return (length content)

modifiedContentLen :: Reader String Int
modifiedContentLen = local ("Prefix " ++) contentLen

printReaderContent :: ReaderT String IO ()
printReaderContent = do
    content <- ask
    liftIO $ putStrLn ("The Reader Content: " ++ content)
