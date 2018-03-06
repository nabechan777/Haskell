module EelRestaurant (run) where

type StartPosition = Int
type GroupSize = Int

run :: IO ()
run = do
    (n, m) <- fmap (toTuple . words) getLine
    a_b <- fmap (map toTuple . map words . lines) getContents
    let seat = replicate n True
        newSeats = checkSeat seat a_b
        result = foldr (\ aSeat acc -> if aSeat then acc else acc + 1) 0 newSeats
    print result

toTuple :: [String] -> (Int, Int)
toTuple [] = error "List length is 2 or more."
toTuple (x:y:_) = (read x, read y)

checkSeat :: [Bool] -> [(GroupSize, StartPosition)] -> [Bool]
checkSeat seat [] = seat
checkSeat seat (x:xs) = if and $ forcusSeats seat then checkSeat newSeats xs else checkSeat seat xs
    where
        seatRange = fst x + snd x - 1
        forcusSeats seat
            | numberOfSeats >= seatRange = take (fst x) $ drop (snd x - 1) seat
            | numberOfSeats < seatRange = take (seatRange - numberOfSeats) seat ++ drop (snd x - 1) seat
                where numberOfSeats = length seat
        newSeats = take (snd x - 1) seat ++ replicate (fst x) False ++ drop seatRange seat
