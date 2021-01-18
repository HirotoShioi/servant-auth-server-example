{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Config where

import           Data.String
import           Database.Persist.Postgresql
import           Servant.Auth.Server

data Env = Env {
    envJWTSettings    :: JWTSettings,
    envCookieSettings :: CookieSettings,
    envDatabasePool   :: ConnectionPool
}

mkEnv :: ConnectionPool -> IO Env
mkEnv pool = do
  -- We *also* need a key to sign the cookies
  myKey <- generateKey
  -- Adding some configurations. 'Cookie' requires, in addition to
  -- CookieSettings, JWTSettings (for signing), so everything is just as before
  let jwtCfg = defaultJWTSettings myKey
  return $ Env jwtCfg defaultCookieSettings pool

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
