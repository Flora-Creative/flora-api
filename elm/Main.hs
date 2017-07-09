module Main where

import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Api                         (app, specs)
import           Config                      (Config (..), Environment (..),
                                              makePool, setLogger)
import           Models                      (doMigrations)
import           Safe                        (readMay)
import           Elm                         (Spec (Spec), specsToDir, toElmDecoderSource,
                                              toElmTypeSource)


-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
    specsToDir [specs] "generated"
