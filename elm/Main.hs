{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api (AppAPI, app)
import Api.ContactForm
import Api.FloraApp
import Config (Config(..), Environment(..), makePool, setLogger)
import Data.Proxy (Proxy(Proxy))
import Data.Text
import Database.Persist.Postgresql (runSqlPool)
import Elm
       (Spec(Spec), specsToDir, toElmDecoderSource, toElmEncoderSource,
        toElmTypeSource)
import Models (IOSApp)
import Network.Wai.Handler.Warp (run)
import Safe (readMay)
import Servant.Elm
import System.Environment (lookupEnv)

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main
    :: IO ()
main = specsToDir [specs] "generated"

specs :: Spec
specs = Spec ["API"] (defElmImports : appsEndpoints <> contactEndpoint)
  where
    appsEndpoints = 
        toElmTypeSource (Proxy :: Proxy IOSApp) :
        toElmDecoderSource (Proxy :: Proxy IOSApp) :
        generateElmForAPIWith elmOpts (Proxy :: Proxy FloraAppAPI)
    contactEndpoint = 
        toElmTypeSource (Proxy :: Proxy ContactForm) :
        toElmEncoderSource (Proxy :: Proxy ContactForm) :
        toElmDecoderSource (Proxy :: Proxy ContactForm) :
        generateElmForAPIWith elmOpts (Proxy :: Proxy ContactApi)

elmOpts :: ElmOptions
elmOpts = 
    defElmOptions
    { urlPrefix = Dynamic
    }