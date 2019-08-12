{-# LANGUAGE OverloadedStrings #-}

module ParserSpec
    ( spec
    ) where

import Test.Hspec
import Lib
import Parser
import Data.Attoparsec.Text

spec :: Spec
spec = do
    describe "labeledDate" $ do
        it "return a data of SimpleDate type when  a date string　with \"date\" label" $ do
            (parseOnly labeledDate "date: 2019/01/01") `shouldBe` (Right SimpleDate { year = 2019, month = 01, day = 01 })

    describe "labeledName" $ do
        it "return string of name when a string with a \"name\" label" $ do
            (parseOnly labeledName "name: \"YamadaTaro\"") `shouldBe` (Right "YamadaTaro")

    describe "labeledGender" $ do
        context "return a data of Gender when a string with a \"gender\" label" $ do
            it "return Male when a string \"gender: male\"" $ do
                (parseOnly labeledGender "gender: male") `shouldBe` (Right Male)
            it "return Female when a string \"gender: female\"" $ do
                (parseOnly labeledGender "gender: female") `shouldBe` (Right Female)
            it "return Unknown when a string \"gender: XXXX\"" $ do
                (parseOnly labeledGender "gender: XXXX") `shouldBe` (Right Unknown)

    describe "labeledBirthday" $ do
        it "return a data of SimpleDate type when  a date string　with \"birthday\" label" $ do
            (parseOnly labeledBirthday "birthday: 2019/01/01") `shouldBe` (Right SimpleDate { year = 2019, month = 01, day = 01 })

    describe "labeledProfile" $ do
        it "Returns composite data when it receives name, date of birth and gender data."　$ do
            (parseOnly labeledProfile "name: \"Yamada Ichiro\" birthday: 2016/11/01 gender: male") `shouldBe` (Right (Profile {name = "Yamada Ichiro", birthday = SimpleDate {year = 2016, month = 11, day = 1}, gender = Male}))
