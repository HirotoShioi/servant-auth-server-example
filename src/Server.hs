{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Server where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8  as LC8
import           Data.Text                   (Text)
import           Database.Persist.Postgresql
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Auth.Server
import           Control.Monad.Logger        (runNoLoggingT)

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

handleProtected :: Servant.Auth.Server.AuthResult Login -> Server ProtectedAPI
handleProtected (Authenticated userInfo) =
         return (getName $ loginUsername userInfo)
    :<|> return (loginEmail userInfo)
handleProtected _ = throwAll err401

handleLogin
    :: Env
    -> LoginMessage
    -> Handler (AcceptHeader NoContent)
handleLogin (Env jwtSettings cookieSettings pool) loginMessage = do
    eUser <- liftIO $ runWithPool pool $ loginUser loginMessage
    case eUser of
        Left e -> throwError err401 { errBody = "Login error: " <> LC8.pack (show e) }
        Right userInfo -> do
            mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings userInfo
            case mApplyCookies of
                Nothing           -> throwError err401
                Just applyCookies -> return $ applyCookies NoContent

handleRegister :: Env -> RegisterMessage -> Handler Login
handleRegister (Env _jwt _cookie pool) registerMessage = do
    eUserInfo <- liftIO $ runWithPool pool $ registerUser registerMessage
    case eUserInfo of
        Left e -> throwError err401 { errBody = "Register error: " <> LC8.pack (show e)}
        Right userInfo -> return userInfo

server :: Env -> Server (API auths)
server env =
         handleProtected
    :<|> handleLogin env
    :<|> handleRegister env

startServer :: IO ()
startServer = do
  pool <- runNoLoggingT $ createPostgresqlPool (mkConnStr dbConfig) 5
  runSqlPool (runMigration migrateAll) pool
  env <- mkEnv pool
  let cfg = (envCookieSettings env) :. (envJWTSettings env) :. EmptyContext
      --- Here is the actual change
      api = Proxy :: Proxy (API '[JWT])
  let port = 7249
  putStrLn $ "Running on Port: " <> show port
  run port $ serveWithContext api cfg (server env)
