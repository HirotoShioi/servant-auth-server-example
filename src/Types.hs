{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Types
    ( Login(..)
    , LoginMessage(..)
    , RegisterMessage(..)
    , Name(..)
    , Password(..)
    ) where

import           RIO

import           Data.Aeson
import           Database.Persist.TH
import           Servant.Auth.Server
import           Test.QuickCheck

newtype Name = Name { getName :: Text }
    deriving (Show, Eq, Ord, Read)

instance Arbitrary Name where
    arbitrary = (Name . fromString) <$> arbitrary

derivePersistField "Name"

newtype Password = Password Text
    deriving (Ord, Read)

instance Show Password where
    show _p = "Password"

instance Eq Password where
    (Password p1) == (Password p2) = p1 == p2

derivePersistField "Password"

data RegisterMessage = RegisterMessage {
        registerUserName :: !Name,
        registerEmail    :: !Text,
        registerPassword :: !Password
    }

instance FromJSON RegisterMessage where
    parseJSON = withObject "RegisterMessage" $ \o -> do
        name <- Name <$> o .: "name"
        email <- o .: "email"
        password <- Password <$> o .: "password"
        return $ RegisterMessage name email password

data Login = Login {
    loginUsername :: !Name,
    loginEmail    :: !Text
} deriving (Eq, Show)

instance ToJSON Login where
    toJSON (Login name email) =
        object [ "name" .= (getName name), "email" .= email]

instance FromJSON Login where
    parseJSON = withObject "Login" $ \o -> do
        name <- Name <$> o .: "name"
        email <- o .: "email"
        return $ Login name email

instance ToJWT Login
instance FromJWT Login

data LoginMessage = LoginMessage {
    loginMessageName     :: !Name,
    loginMessagePassword :: !Password
}

instance Arbitrary Login where
    arbitrary = Login <$> arbitrary <*> arbitrary

instance FromJSON LoginMessage where
    parseJSON = withObject "LoginMessage" $ \o -> do
        name <- Name <$> o .: "name"
        password <- Password <$> o .: "password"
        return $ LoginMessage name password
