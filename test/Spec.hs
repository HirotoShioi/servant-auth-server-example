{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           RIO

import           Data.Aeson
import           Server
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Test.QuickCheck
import           Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Application" $ do
    describe "Login" $
        prop "Should be able to login" $ \(login :: Login) -> decode (encode login) === Just login

-- routeSpec :: Spec
-- routeSpec = with (mkApp) $ do
--     describe "GET /users" $ do
--         it "responds with 200" $ do
--             get "/users" `shouldRespondWith` 200
--         it "responds with [User]" $ do
--             let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
--             get "/users" `shouldRespondWith` users
