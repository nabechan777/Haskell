module Main where

import Data.List (sortBy, intersperse)
import Data.Maybe (catMaybes)

import FRP.Yampa
import FRP.Yampa.Utilities
import FRP.Yampa.Internals

import ElevetorImpl

smplPer = 0.01

lbps :: SF a (Event ())
lbps = afterEach [(3.0, ()), (2.0, ()), (50.0, ())]

rbps :: SF a (Event ())
rbps = afterEach [(20.0, ()), (2.0, ()), (18.0, ()), (15.001, ())]

data State = Stopped | GoingUp | GoingDown deriving (Eq)

testElevetor :: Time -> [(Time, ((Event (), Event ()), Position))]
testElevetor t_max = takeWhile ((<= t_max) . fst) tions
    where
        tions = embed (localTime &&& ((lbps &&& rbps >>^ dup) >>> second elevator)) (deltaEncode smplPer (repeat ()))

findEvents :: [(Time, ((Event (), Event ()), Position))] -> [(Time, Position, String)]
findEvents [] = []
findEvents tios@((_, (_, y)) : _) = feAux Stopped y tios
    where
        feAux _ _ [] = []
        feAux sPre yPre ((t, ((lbp, rbp), y)) : tios') =
            if not (null message)
                then (t, y, message) : feAux s y tios'
                else feAux s y tios'
            where
                s = if y == yPre then Stopped else if yPre < y then GoingUp else GoingDown
                ms = if s /= sPre
                    then case s of
                        Stopped -> Just "elevator stopped"
                        GoingUp -> Just "elevator started going up"
                        GoingDown -> Just "elevator started going down"
                    else
                        Nothing
                mu = if isEvent lbp
                    then Just "up button pressed"
                    else Nothing
                md = if isEvent rbp
                    then Just "down button pressed"
                    else Nothing
                message = concat (intersperse ", " (catMaybes [ms, mu, md]))

formatEvents :: (Time, Position, String) -> String
formatEvents (t, y, m) = "t = " ++ t' ++ ",\ty = " ++ y' ++ ":\t" ++ m
    where
        t' = show (fromIntegral (round (t * 100)) / 100)
        y' = show (fromIntegral (round (y * 100)) / 100)

ppEvents [] = return ()
ppEvents (e:es) = putStrLn (formatEvents e) >> ppEvents es

main = ppEvents (findEvents (testElevetor 100))
