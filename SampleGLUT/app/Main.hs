module Main where

import qualified Sample1 as S1
import qualified Sample2 as S2
import qualified Sample3 as S3
import qualified Sample4 as S4
import qualified Sample5 as S5
import qualified Sample6 as S6

run :: String -> IO ()
run "1" = S1.run
run "2" = S2.run
run "3" = S3.run
run "4" = S4.run
run "5" = S5.run
run "6" = S6.run
run  _  = error "Undefined Sample Number."

main :: IO ()
main = putStrLn "Input Sample Number" >> getLine >>= (\x -> run x)
