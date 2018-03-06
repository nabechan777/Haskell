module RockPaperScissors (run) where

import Control.Monad

run :: IO ()
run = do
    (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
    rhs <- getLine
    let
        cmb = combination rhs m [(0, 0)]
        result = maximum $ map fst $ filter (\x -> snd x == m) cmb
    print result

combination :: [Char] -> Int -> [(Int, Int)] -> [(Int, Int)]
combination "" _ xs = xs
combination (rh:rhs) m xs = combination rhs m $ combinationImpl rh m xs

combinationImpl :: Char -> Int -> [(Int, Int)] -> [(Int, Int)]
combinationImpl rh m xs = do
    oh <- ['G', 'P', 'C']
    x <- xs
    guard(snd x <= m)
    let win = if judgement rh oh
            then fst x + 1
            else fst x
        fin = case oh of
            'G' -> snd x
            'P' -> snd x + 5
            'C' -> snd x + 2
    return (win, fin)

judgement :: Char -> Char -> Bool
judgement 'C' 'G' = True
judgement 'G' 'P' = True
judgement 'P' 'C' = True
judgement _ _ = False
