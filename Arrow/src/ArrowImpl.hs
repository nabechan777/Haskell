{-# LANGUAGE Arrows #-}

module ArrowImpl where

import Control.Arrow
import Control.Monad
import qualified Control.Category as Cat
import Data.List
import Data.Maybe
import System.Random

-- 状態を維持するArrow
newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }

instance Cat.Category Circuit where
    id = Circuit $ \a -> (Cat.id, a)
    (.) = dot
        where
            (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
                let (cir1', b) = cir1 a
                    (cir2', c) = cir2 b
                in (cir2' `dot` cir1', c)

instance Arrow Circuit where
    arr f = Circuit $ \a -> (arr f, f a)
    first (Circuit cir) = Circuit $ \(b, d) ->
        let (cir', c) = cir b
        in (first cir', (c, d))

instance ArrowChoice Circuit where
    left orig@(Circuit cir) = Circuit $ \ebd -> case ebd of
        Left b -> let (cir', c) = cir b in (left cir', Left c)
        Right d -> (left orig, Right d)

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit cir = snd . mapAccumL unCircuit cir

accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \input ->
    let (output, acc') = input `f` acc
    in (accum acc' f, output)

accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\a b -> let b' = a `f` b in (b', b'))

total :: Num a => Circuit a a
total = accum' 0 (+)

mean1 :: Fractional a => Circuit a a
mean1 = total &&& (const 1 ^>> total) >>> arr (uncurry (/))

mean2 :: Fractional a => Circuit a a
mean2 = proc value -> do
    t <- total -< value
    n <- total -< 1
    returnA -< t / n

generator :: Random a => (a, a) -> StdGen -> Circuit () a
generator range rng = accum rng $ \ () rng -> randomR range rng

dictionary = ["dog", "bird", "dog"]

pickWord :: StdGen -> Circuit () String
pickWord rng = proc () -> do
    idx <- generator (0, length dictionary-1) rng -< ()
    returnA -< dictionary !! idx

oneShot :: Circuit () Bool
oneShot = accum True $ \_ acc -> (acc, False)

delayedEcho :: a -> Circuit a a
delayedEcho acc = accum acc (flip (,))

getWord :: StdGen -> Circuit () String
getWord rng = proc () -> do
    firstTime <- oneShot -< ()
    mPicked <- if firstTime
        then do
            picked <- pickWord rng -< ()
            returnA -< Just picked
        else returnA -< Nothing
    mWord <- accum' Nothing mplus -< mPicked
    returnA -< fromJust mWord

attempts :: Int
attempts = 5

livesLeft :: Int -> String
livesLeft hung = "Lives: [" ++ replicate (attempts - hung) '#' ++ replicate hung ' ' ++ "]"

hangman :: StdGen -> Circuit String (Bool, [String])
hangman rng = proc userInput -> do
    word <- getWord rng -< ()
    let letter = listToMaybe userInput
    guessed <- updateGuess -< (word, letter)
    hung <- updateHung -< (word, letter)
    end <- delayedEcho True -< not (word == guessed || hung >= attempts)
    let result = if word == guessed
                    then [guessed, "You won!!"]
                    else if hung >= attempts
                        then [guessed, livesLeft hung, "You died!"]
                        else [guessed, livesLeft hung]
    returnA -< (end, result)
    where
        updateGuess :: Circuit (String, Maybe Char) String
        updateGuess = accum' (repeat '_') $ \(word, letter) guess ->
            case letter of
                Just l -> map (\(w, g) -> if w == l then w else g) (zip word guess)
                Nothing -> take (length word) guess

        updateHung :: Circuit (String, Maybe Char) Int
        updateHung = proc (word, letter) -> do
            total -< case letter of
                Just l -> if l `elem` word then 0 else 1
                Nothing -> 0
