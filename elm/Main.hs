{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Api                         (AppAPI, app)
import           Api.ContactForm
import           Api.FloraApp
import           Config                      (Config (..), Environment (..),
                                              makePool, setLogger)
import           Data.Proxy                  (Proxy (Proxy))
import           Data.Text
import           Elm                         (Spec (Spec), specsToDir,
                                              toElmDecoderSource,
                                              toElmEncoderSource,
                                              toElmTypeSource)
import           Models                      (IOSApp)
import           Safe                        (readMay)
import           Servant.Elm


-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = specsToDir [specs] "generated"

specs :: Spec
specs =
    Spec ["API"]
         (defElmImports
          : toElmTypeSource    (Proxy :: Proxy ContactForm)
          : toElmDecoderSource (Proxy :: Proxy ContactForm)
          : toElmTypeSource    (Proxy :: Proxy IOSApp)
          : toElmDecoderSource (Proxy :: Proxy IOSApp)
          : generateElmForAPIWith elmOpts  (Proxy :: Proxy FloraAppAPI))

elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Dynamic }
