module HelloWorld where

import Data.Int(Int32)
import Database.Relational.Query

run :: IO ()
run = putStrLn $ show helloWorld ++ ";"

hello :: Relation () (Int32, String)
hello = relation $ return (value 0 >< value "Hello")

world :: Relation () (Int32, String)
world = relation $ return (value 0 >< value " World")

helloWorld :: Relation () (Int32, (String, String))
helloWorld = relation $ do
    h <- query hello
    w <- query world
    on $ h ! fst' .=. w ! fst'
    return $ h ! fst' >< (h ! snd' >< w ! snd')
