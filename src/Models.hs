{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Control.Monad.Reader
import Data.Aeson
import Data.Text
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)
import Config
import Elm
import Elm.Export.Persist
import Elm.Export.Persist.BackendKey ()
import Servant ((:<|>), (:>), Get, JSON, Post, ReqBody)

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
Floraapps json
    appName String
    images [String]
    videoLinks [String]
    itunesUrl String
    appDescription String
    backgroundColor String
    foregroundColor String
    auIdentifier String
    appIcon String
    shortName String
    deriving Show Generic
|]

instance ElmType Floraapps

deriving instance ElmType FloraappsId

data IOSApp = IOSApp
    { appName :: String
    , images :: [String]
    , videoLinks :: [String]
    , itunesUrl :: String
    , appDescription :: String
    , backgroundColor :: String
    , foregroundColor :: String
    , auIdentifier :: String
    , appIcon :: String
    , shortName :: String
    } deriving (Eq,Show,Generic)

instance ToJSON IOSApp

instance FromJSON IOSApp

instance ElmType IOSApp

appFromEntity :: Entity Floraapps -> IOSApp
appFromEntity entity = 
    IOSApp
    { appName = floraappsAppName $ entityVal entity
    , images = floraappsImages $ entityVal entity
    , videoLinks = floraappsVideoLinks $ entityVal entity
    , itunesUrl = floraappsItunesUrl $ entityVal entity
    , appDescription = floraappsAppDescription $ entityVal entity
    , backgroundColor = floraappsBackgroundColor $ entityVal entity
    , foregroundColor = floraappsForegroundColor $ entityVal entity
    , auIdentifier = floraappsAuIdentifier $ entityVal entity
    , appIcon = floraappsAppIcon $ entityVal entity
    , shortName = floraappsShortName $ entityVal entity
    }

doMigrations :: SqlPersistM ()
doMigrations = runMigration migrateAll

runDb
    :: (MonadReader Config m, MonadIO m)
    => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool