module SampleImpl where

import Data.Maybe
import Control.Monad
import Control.Applicative
import Graphics.UI.WX
import Reactive.Banana
import Reactive.Banana.WX

run :: IO ()
run = start $ do
    f <- frame [text := "Arithmetic"]                      :: IO (Frame ())
    input1 <- entry f []                                   :: IO (TextCtrl ())
    input2 <- entry f []                                   :: IO (TextCtrl ())
    inputText1 <- sequence $ replicate 3 (staticText f []) :: IO ([StaticText ()])
    inputText2 <- sequence $ replicate 3 (staticText f []) :: IO ([StaticText ()])
    outputs <- sequence $ replicate 4 (staticText f [])    :: IO ([StaticText ()])

    let operand1 = widget input1                                                                :: Layout
        operand2 = widget input2                                                                :: Layout
        operandText1 = map ((minsize (sz 101 20)) . widget) inputText1                          :: [Layout]
        operandText2 = map ((minsize (sz 101 20)) . widget) inputText2                          :: [Layout]
        operators = map label ["+", "-", "*", "/"]                                              :: [Layout]
        values = map ((minsize (sz 40 20)) . widget) outputs                                    :: [Layout]

        rowScalton = \a b c d -> row 10 [a, b, c, label "=", d]

        rows :: ZipList Layout
        rows = rowScalton <$> ZipList (operand1 : operandText1)
                          <*> ZipList operators
                          <*> ZipList (operand2 : operandText2)
                          <*> ZipList values

        column1 = column 10 $ getZipList rows

    set f [layout := margin 10 column1]                                                          :: IO ()

    let networkDescription :: MomentIO ()
        networkDescription = do
            -- テキストフィールドに入力された際に対応する動作
            binput1 <- behaviorText input1 "" :: MomentIO (Behavior String)
            binput2 <- behaviorText input2 "" :: MomentIO (Behavior String)

            let readNumber :: String -> Maybe Int
                readNumber s = listToMaybe [x | (x, "") <- reads s]

                showNumber :: Maybe Int -> String
                showNumber = maybe "--" show

                resultAdd :: Behavior (Maybe Int)
                resultAdd = f <$> binput1 <*> binput2
                    where
                        f x y = liftA2 (+) (readNumber x) (readNumber y)

                resultSub :: Behavior (Maybe Int)
                resultSub = f <$> binput1 <*> binput2
                    where
                        f x y = liftA2 (-) (readNumber x) (readNumber y)

                resultMul :: Behavior (Maybe Int)
                resultMul = f <$> binput1 <*> binput2
                    where
                        f x y = liftA2 (*) (readNumber x) (readNumber y)

                resultDiv :: Behavior (Maybe Int)
                resultDiv = f <$> binput1 <*> binput2
                    where
                        f _ "0" = Nothing
                        f x y = liftA2 (div) (readNumber x) (readNumber y)

            mapM_ (\x -> sink x [text :== binput1]) inputText1                                               :: MomentIO ()
            mapM_ (\x -> sink x [text :== binput2]) inputText2                                               :: MomentIO ()
            let sinks = map (\x -> sink x) outputs                                                           :: [[Prop' (StaticText ())] -> MomentIO ()]
                props = map (\f -> [text :== showNumber <$> f]) [resultAdd, resultSub, resultMul, resultDiv] :: [[Prop' (StaticText ())]]
            zipWithM_ (\f x -> f x) sinks props                                                              :: MomentIO ()

    network <- compile networkDescription :: IO EventNetwork
    actuate network                       :: IO ()
