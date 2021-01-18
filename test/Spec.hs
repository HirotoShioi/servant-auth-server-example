{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import User
import Data.Aeson
import Test.QuickCheck

main :: IO ()
main = putStrLn "Test not implemented"

-- spec :: Spec
-- spec = with (return app) $ do
--     describe "GET /users" $ do
--         it "responds with 200" $ do
--             get "/users" `shouldRespondWith` 200
--         it "responds with [User]" $ do
--             let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
--             get "/users" `shouldRespondWith` users

propUserInfoRoundTrip :: UserInfo -> Property
propUserInfoRoundTrip userInfo = decode (encode userInfo) === Just userInfo