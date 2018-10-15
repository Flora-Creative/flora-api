{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Api.FloraApp where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.), SelectOpt(..))
import           Network.Wai                 (Application)
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Config                      (AppT (..), Config (..))
import           Models

type FloraAppAPI =
          Get '[JSON] [IOSApp]
          :<|>  Capture "name" String :> Get '[JSON]  IOSApp


floraAppAPI :: Proxy FloraAppAPI
floraAppAPI = Proxy

-- | The server that runs the UserAPI
floraAppServer :: (MonadIO m) => ServerT FloraAppAPI (AppT m)
floraAppServer = allFloraApps
                 :<|> singleFloraApp

-- | Returns all users in the database.
allFloraApps :: (MonadIO m) => AppT m [IOSApp]
allFloraApps = do
    storedApps <- runDb (selectList [] [])
    let apps = map appFromEntity storedApps
    return apps

-- | Returns an app by name or throws a 404 error.
singleFloraApp :: (MonadIO m) => String -> AppT m IOSApp
singleFloraApp str = do
    maybeEntity <- runDb (selectFirst [FloraappsShortName ==. str] [Asc FloraappsId])
    let maybeApp = fmap appFromEntity maybeEntity
    case maybeApp of
         Nothing ->
            throwError err404
         Just floraApp ->
            return floraApp
