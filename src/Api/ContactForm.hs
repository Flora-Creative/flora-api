{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api.ContactForm where

import Config (AppT(..), Config(..), SMTPServer(..))
import Control.Monad.Except
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Data.Aeson
import Data.Text as T
import Elm
import GHC.Generics
import Network.HaskellNet.Auth (AuthType(LOGIN))
import Network.HaskellNet.SMTP
import Network.HaskellNet.SMTP.SSL as SMTP
import Network.Socket
import Servant
import Servant.JS (vanillaJS, writeJSForAPI)
import qualified Text.Blaze.Html
import Text.Blaze.Html5

data ContactForm = ContactForm
    { name :: String
    , email :: String
    , subject :: String
    , message :: String
    } deriving (Eq,Show,Generic)

instance FromJSON ContactForm

instance ToJSON ContactForm

instance ElmType ContactForm

contactApi :: Proxy ContactApi
contactApi = Proxy

contactHandlerWithServer
    :: (MonadIO m)
    => SMTPServer -> ServerT ContactApi (AppT m)
contactHandlerWithServer = contactFormHandlerWithServer

contactFormHandlerWithServer
    :: (MonadIO m)
    => SMTPServer -> ContactForm -> AppT m ContactForm
contactFormHandlerWithServer server contactForm = do
    liftIO $ sendEmail server contactForm
    return contactForm

sendEmail :: SMTPServer -> ContactForm -> IO ()
sendEmail server form = do
    let from = username server
    let to = username server
    let subject = "email subject"
    let body = "email body"
    let htmlBody = ""
    putStrLn $
        "Attempting to authenticate:" <> username server <> ":" <> password server
    doSMTPPort (host server) (port server) $
        \conn -> do
            authSuccess <- 
                SMTP.authenticate LOGIN (username server) (password server) conn
            if authSuccess
                then sendMimeMail to from subject body htmlBody [] conn
                else putStrLn "Authentication failed."

type ContactApi = "contact" :> ReqBody '[JSON] ContactForm :> Post '[JSON] ContactForm