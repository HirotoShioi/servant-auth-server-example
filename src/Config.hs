{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Config where

import           RIO

import           Database.Persist.Postgresql (ConnectionPool, ConnectionString)
import           Servant.Auth.Server

data Config = Config {
  cfgPort :: !Int
}

defaultConfig :: Config
defaultConfig = Config 8080

data Env = Env {
    envLogFunc        :: !LogFunc,
    envJWTSettings    :: !JWTSettings,
    envCookieSettings :: !CookieSettings,
    envDatabasePool   :: !ConnectionPool
}

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y})

-- | Database configuration
data DBConfig = DBConfig {
      cfgHost     :: !String
    , cfgDBName   :: !String
    , cfgUser     :: !String
    , cfgPassword :: !String
    , cfgDBPort   :: !Int
    }

-- | Create 'ConnectionString' based upon 'DBConfig'
mkConnStr :: DBConfig -> ConnectionString
mkConnStr DBConfig{..} = fromString $
    mconcat ["host="
           , cfgHost
           , " dbname="
           , cfgDBName
           , " user="
           , cfgUser
           , " password="
           , cfgPassword
           , " port="
           , show cfgDBPort
           ]

dbConfig :: DBConfig
dbConfig = DBConfig
    { cfgHost = "localhost"
    , cfgDBName = "perservant"
    , cfgUser = "test"
    , cfgPassword = "test"
    , cfgDBPort = 5432
    }
