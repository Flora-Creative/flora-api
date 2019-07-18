{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api (app, appAPI)
import Config (Config(..), Environment(..), SMTPServer(..), makePool, setLogger)
import Database.Persist.Postgresql (runSqlPool)
import Models (doMigrations)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  ( cors
  , corsMethods
  , corsRequestHeaders
  , simpleCorsResourcePolicy
  )
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Safe (readMay)
import System.Environment (lookupEnv)

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
  env <- lookupSetting "ENV" Development
  runport <- lookupSetting "PORT" 1234
  pool <- makePool env
  smtpHost <- lookupSettingOrError "SMTPHOST"
  smtpPort <- lookupSettingOrError "SMTPPORT"
  smtpUser <- lookupSettingOrError "SMTPUSER"
  smtpPass <- lookupSettingOrError "SMTPPASS"
  let cfg =
        Config
          { getPool = pool
          , getEnv = env
          , getSMTPServer =
              SMTPServer smtpHost (read smtpPort) smtpUser smtpPass
          }
      logger = setLogger env
      providePreFlightHeaders = provideOptions appAPI
      policy = simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}
      customCors = cors (const $ Just policy)
  putStrLn ("Environment: " ++ show env)
  putStrLn ("Port: " ++ show runport)
  putStrLn ("SMTP Host: " ++ smtpHost)
  putStrLn ("SMTP Port: " ++ smtpPort)
  run runport . logger . customCors . providePreFlightHeaders . app $ cfg

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing -> return def
    Just str -> maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $
      mconcat ["Failed to read [[", str, "]] for environment variable ", env]

lookupSettingOrError :: Read a => String -> IO a
lookupSettingOrError env = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing -> handleFailedRead env
    Just str -> maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $
      mconcat ["Failed to read [[", str, "]] for environment variable ", env]
