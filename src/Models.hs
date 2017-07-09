{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving         #-}


module Models where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Text
import           Database.Persist.Sql
import           Database.Persist.TH
import           GHC.Generics         (Generic)

import           Config
import           Elm
import           Servant      ((:<|>), (:>), ReqBody, Post, Get, JSON)
import           Elm.Export.Persist
import           Elm.Export.Persist.BackendKey ()

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
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

newtype FloraApp = EntId Floraapps

doMigrations :: SqlPersistM ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
