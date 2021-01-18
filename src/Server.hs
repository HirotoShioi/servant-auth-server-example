{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Server where

import           RIO

import           Control.Monad.Logger                (runNoLoggingT)
import qualified Data.ByteString.Lazy.Char8          as LC8
import           Database.Persist.Postgresql
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Auth.Server

import           Config
import           Methods
import           Types

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

type AcceptHeader returnContent =
    Headers
        '[ Header "Set-Cookie" SetCookie
         , Header "Set-Cookie" SetCookie
         ]
         returnContent

type LoginAPI = "login"
    :> ReqBody '[JSON] LoginMessage
    :> Verb 'POST 204 '[JSON] (AcceptHeader NoContent)

type RegisterAPI = "register" :> ReqBody '[JSON] RegisterMessage :> Post '[JSON] Login

type ProtectedAPI
   = "name" :> Get '[JSON] Text
 :<|> "email" :> Get '[JSON] Text

type API auths =
    (Auth auths Login :> ProtectedAPI)
    :<|> LoginAPI
    :<|> RegisterAPI

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

handleProtected :: Servant.Auth.Server.AuthResult Login -> ServerT ProtectedAPI App
handleProtected (Authenticated userInfo) =
         return (getName $ loginUsername userInfo)
    :<|> return (loginEmail userInfo)
handleProtected _ = throwAll err401

handleLogin
    :: LoginMessage
    -> App (AcceptHeader NoContent)
handleLogin loginMessage = do
    Env{..} <- ask
    eUser <- liftIO $ runWithPool envDatabasePool $ loginUser loginMessage
    case eUser of
        Left e -> throwError err401 { errBody = "Login error: " <> LC8.pack (show e) }
        Right userInfo -> do
            mApplyCookies <- liftIO $ acceptLogin envCookieSettings envJWTSettings userInfo
            case mApplyCookies of
                Nothing           -> throwError err401
                Just applyCookies -> return $ applyCookies NoContent

handleRegister :: RegisterMessage -> App Login
handleRegister registerMessage = do
    Env{..} <- ask
    eUserInfo <- liftIO $ runWithPool envDatabasePool $ registerUser registerMessage
    case eUserInfo of
        Left e -> throwError err401 { errBody = "Register error: " <> LC8.pack (show e)}
        Right userInfo -> return userInfo

server :: ServerT (API auths) App
server =
         handleProtected
    :<|> handleLogin
    :<|> handleRegister

type App = ReaderT Env Servant.Handler

nt :: Env -> App a -> Servant.Handler a
nt ctx x = runReaderT x ctx

startServer :: IO ()
startServer = do
  envDatabasePool <- runNoLoggingT $ createPostgresqlPool (mkConnStr dbConfig) 5
  runSqlPool (runMigration migrateAll) envDatabasePool
  myKey <- generateKey
  let envJWTSettings = defaultJWTSettings myKey
  let envCookieSettings = defaultCookieSettings
  let env = Env {..}
  let cfg = envCookieSettings :. envJWTSettings :. EmptyContext
      --- Here is the actual change
      api = Proxy :: Proxy (API '[JWT])
  let port = 7249
  run port 
    $ serveWithContext api cfg 
    $ hoistServerWithContext
        api
        (Proxy :: Proxy '[JWTSettings, CookieSettings])
        (nt env)
        server
