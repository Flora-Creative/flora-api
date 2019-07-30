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
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy as T
import Elm
import GHC.Generics
import Network.HaskellNet.Auth (AuthType(LOGIN))
import Network.HaskellNet.SMTP ()
import Network.HaskellNet.SMTP.SSL as SSL
import Network.HaskellNet.SSL (Settings)
import Network.Socket
import Servant
import Servant.JS (vanillaJS, writeJSForAPI)
import qualified Text.Blaze.Html
import Text.Blaze.Html5

data ContactForm = ContactForm
    { origin :: String  -- Where it came from, should be "Phlox Phaser" or "Website"
    , name :: String
    , email :: String
    , subject :: String
    , message :: String
    , leaveMeBlank :: Maybe String
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
    liftIO $ forkIO (sendEmail server contactForm)
    return contactForm

sendEmail :: SMTPServer -> ContactForm -> IO ()
sendEmail (SMTPServer host port username password) form = 
    case leaveMeBlank form of
        Just _ -> putStrLn "Received honeypot request"
        Nothing -> 
            doSMTPSSLWithSettings host sslSettings $
            \conn -> do
                authSuccess <- SSL.authenticate LOGIN username password conn
                if authSuccess
                    then do
                        print form
                        sendPlainTextMail
                            "support@flora-creative.com"
                            username
                            emailSubject
                            emailBody
                            conn
                    else putStrLn "Authentication failed."
            where sslSettings = Settings port 10000 False False
                  emailBody = messageFromContactForm form
                  emailSubject = "Automated message received from " <> origin form

messageFromContactForm :: ContactForm -> Text
messageFromContactForm (ContactForm _ name email subject message _) = 
    T.pack $
    "Message received from: " <> name <> "\n Email address (replyto:): " <> email <>
    "\n Subject of inquiry: " <>
    subject <>
    "\n Message: " <>
    message

type ContactApi = "contact" :> ReqBody '[JSON] ContactForm :> Post '[JSON] ContactForm