{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api
  (app
  ,appAPI)
  where

import Api.ContactForm
import Api.FloraApp
import Config (AppT(..), Config(..))
import Control.Monad.Except
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Control.Natural
import Data.Int (Int64)
import Data.Text
import Database.Persist.Postgresql
       (Entity(..), fromSqlKey, insert, selectFirst, selectList, (==.))
import GHC.Generics (Generic)
import Models
import Network.Wai (Application)
import Servant

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer
    :: Config -> Server FloraAppAPI
appToServer cfg = hoistServer floraAppAPI (convertApp cfg) floraAppServer

contactToServer :: Config -> Server ContactApi
contactToServer cfg = 
    hoistServer contactApi (convertApp cfg) $
    contactHandlerWithServer (getSMTPServer cfg)

-- | This function converts our 'App' monad into the @ExceptT ServantErr
-- IO@ monad that Servant's 'enter' function needs in order to run the
-- application. The ':~>' type is a natural transformation, or, in
-- non-category theory terms, a function that converts two type
-- constructors without looking at the values in the types.
-- convertApp :: Config -> App :~> ExceptT ServantErr IO
-- convertApp cfg = NT (flip runReaderT cfg . runApp)
convertApp
    :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

-- | Since we also want to provide a minimal front end, we need to give
-- Servant a way to serve a directory with HTML and JavaScript. This
-- function creates a WAI application that just serves the files out of the
-- given directory.
files
    :: Server Raw
files = serveDirectoryFileServer "assets"

-- | Just like a normal API type, we can use the ':<|>' combinator to unify
-- two different APIs and applications. This is a powerful tool for code
-- reuse and abstraction! We need to put the 'Raw' endpoint last, since it
-- always succeeds.
type APIWithResources = FloraAppAPI :<|> ContactApi :<|> Raw

-- A sneaky thing here is that our Raw endpoint being last will return NoContent
-- for every unknown route, which will prevent us from generating an OPTIONS route
-- via introspection - in the exposed interface we will omit the Raw endpoint
type APIWithoutResources = FloraAppAPI :<|> ContactApi

appAPI :: Proxy APIWithoutResources
appAPI = Proxy

completeAPIWithResources :: Proxy APIWithResources
completeAPIWithResources = Proxy

-- | Finally, this function takes a configuration and runs our 'UserAPI'
-- alongside the 'Raw' endpoint that serves all of our files.
app
    :: Config -> Application
app cfg = 
    serve
        completeAPIWithResources
        (appToServer cfg :<|> contactToServer cfg :<|> files)