{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api (app, appAPI)
import Config
       (Config(..), Environment(..), SMTPServer(..), makePool,
        makeSMTPServer, setLogger)
import Database.Persist.Postgresql (runSqlPool)
import Models (doMigrations)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
       (cors, corsMethods, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Safe (readMay)
import System.Environment (lookupEnv)

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main
    :: IO ()
main = do
    env <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 1234
    pool <- makePool env
    smtpServer <- makeSMTPServer env
    let cfg = 
            Config
            { getPool = pool
            , getEnv = env
            , getSMTPServer = smtpServer
            }
        logger = setLogger env
        providePreFlightHeaders = provideOptions appAPI
        policy = 
            simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"]
            }
    putStrLn ("Environment: " ++ show env)
    putStrLn ("Port: " ++ show port)
    putStrLn ("SMTP: " ++ show smtpServer)
    run port . logger . cors (const $ Just policy) . providePreFlightHeaders . app $
        cfg

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting
    :: Read a
    => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing -> return def
        Just str -> maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str = 
        error $ mconcat ["Failed to read [[", str, "]] for environment variable ", env]