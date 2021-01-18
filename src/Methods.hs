{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Methods
    ( runWithPool
    , migrateAll
    , registerUser
    , loginUser
    , UserError(..)
    , User(..)
    , UserId
    ) where

import           RIO

import           Control.Monad.Except
import           Data.Maybe
import           Data.Text
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Name
    UniqueUserName name
    email Text
    password Password
    deriving Show
|]

data UserError
    = UserNotFound
    | InvalidPassword
    | UserAlreadyExists
    deriving Show

runWithPool :: ConnectionPool -> SqlPersistM a -> IO a
runWithPool pool action = runSqlPersistMPool action pool

registerUser :: RegisterMessage -> SqlPersistM (Either UserError Login)
registerUser (RegisterMessage name email password) = runExceptT $ do
    mUser <- liftPersist $ getBy $ UniqueUserName name
    if isJust mUser
        then throwError UserAlreadyExists
        else do
            let person = User name email password
            _eUser <- liftPersist $ insertEntity person
            return $ Login name email

loginUser :: LoginMessage -> SqlPersistM (Either UserError Login)
loginUser (LoginMessage username password) = runExceptT $ do
    mUser <- liftPersist $ getBy $ UniqueUserName username
    case mUser of
        Nothing -> throwError UserNotFound
        Just (Entity _id user) ->
            if password /= (userPassword user)
                then throwError InvalidPassword
                else return $ Login username (userEmail user)
