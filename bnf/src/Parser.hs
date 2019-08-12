{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Applicative
import           Control.Monad.Trans.Resource
import           Data.Attoparsec.Text
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Binary          hiding (mapM_)
import           Data.Conduit.Text
import           Data.Functor
import           Data.Text                    (Text)
import           Data.Text.IO

import           Lib

simpleDate :: Parser SimpleDate
simpleDate =
    SimpleDate <$>
        (read <$> count 4 digit) <* char '/' <*>
        (read <$> count 2 digit) <* char '/' <*>
        (read <$> count 2 digit)

quotedText :: Parser Text
quotedText = char '"' *> takeTill (== '"') <* char '"'

parsingGender :: Parser Gender
parsingGender =
    ("male" <$ pure Male) <|> ("female" <$ pure Female) <|> pure Unknown

labeledName :: Parser Text
labeledName = "name: " *> quotedText

labeledDate :: Parser SimpleDate
labeledDate = "date: " *> simpleDate

labeledBirthday :: Parser SimpleDate
labeledBirthday = "birthday: " *> simpleDate

labeledGender :: Parser Gender
labeledGender = "gender: " *> parsingGender

labeledProfile :: Parser Profile
labeledProfile =
    Profile <$>
        labeledName <* space <*>
        labeledBirthday <* space <*>
        labeledGender

csvProfile :: Parser Profile
csvProfile =
    Profile <$>
        quotedText <* char ',' <*>
        simpleDate <* char ',' <*>
        parsingGender

csvProfileSet :: Parser [Profile]
csvProfileSet = many $ csvProfile <* endOfLine

-- 非推奨APIを使用しているため、改善する必要がある。（$$と=$）
-- parseAndShow :: FilePath -> IO ()
-- parseAndShow p = mapM_ print =<< runResourceT $ (sourceFile p $= (decode utf8 $$ sinkParser csvProfileSet))
