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
import           Data.Proxy                  (Proxy (Proxy))
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.), SelectOpt(..))
import           Network.Wai                 (Application)
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Config                      (App (..), Config (..))
import           Models
import           Elm.Export.Persist
import           Elm.Export.Persist.BackendKey ()
import           Elm                         (Spec (Spec), specsToDir, toElmTypeSource,
                                              toElmDecoderSource, toElmEncoderSource)
import           Servant.Elm


type FloraAppAPI =
          Get '[JSON] [Entity Floraapps]
          :<|>  Capture "name" String :> Get '[JSON] (Entity Floraapps)

type FloraAppElmAPI =
          Get '[JSON] [EntId Floraapps]
          :<|>  Capture "name" String :> Get '[JSON] (EntId Floraapps)

-- | The server that runs the UserAPI
floraAppServer :: ServerT FloraAppAPI App
floraAppServer = allFloraApps
                 :<|> singleFloraApp

-- | Returns all users in the database.
allFloraApps :: App [Entity Floraapps]
allFloraApps =
     runDb (selectList [] [])

-- | Returns an app by name or throws a 404 error.
singleFloraApp :: String -> App (Entity Floraapps)
singleFloraApp str = do
    maybeApp <- runDb (selectFirst [FloraappsShortName ==. str] [Asc FloraappsId])
    case maybeApp of
         Nothing ->
            throwError err404
         Just floraApp ->
            return floraApp
