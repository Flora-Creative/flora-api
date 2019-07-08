{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api (app)
import Api.ContactForm
import Api.FloraApp
import Elm
       (Spec(Spec), specsToDir, toElmDecoderSource, toElmEncoderSource,
        toElmTypeSource)
import Models (IOSApp)
import Servant.Elm

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