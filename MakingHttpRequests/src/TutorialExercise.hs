#!/usr/bin/env stack
-- stack script --resolver lts-9.21
{-# LANGUAGE OverloadedStrings #-}
module TutorialExercise
    ( basicUsage
    , receivingJSON
    , buildingRequest
    ) where

import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = basicUsage

-- レスポンスをByteStringで取得
basicUsage :: IO ()
basicUsage = do
    -- サンプルページへリクエストを送信し、レスポンスを変数に束縛
    response <- httpLBS "http://httpbin.org/get"

    -- HTTPステータス, コンテンツタイプ, ボディーを出力する。
    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response

-- レスポンスをFromJSONインスタンスで取得
receivingJSON :: IO ()
receivingJSON = do
    response <- httpJSON "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)

-- リクエストを組み立てる。
buildingRequest :: IO ()
buildingRequest = do
    request' <- parseRequest "POST http://httpbin.org/post"
    let request
            = setRequestMethod "PUT"
            $ setRequestPath "/put"
            $ setRequestQueryString [("hello", Just "world")]
            $ setRequestBodyLBS "This is my request body"
            $ setRequestSecure True
            $ setRequestPort 443
            $ request'
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
