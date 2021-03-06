{-# LANGUAGE OverloadedStrings #-}

module DB where

import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString()
import Data.ByteString.Char8
import Hasql (PoolSettings, acquirePool, poolSettings)
import Hasql.Postgres
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

import Types (DB, AppM)


-- tag::DBAll[]
poolConfig :: PoolSettings
poolConfig = fromMaybe                              -- <1>
  (error "Incorrect DB pooling settings")           -- <2>
  (poolSettings 5 10)                               -- <3>

getPgConfig :: IO Settings
getPgConfig = do
  user <- envDefault "DBUSER" "postgres"
  pass <- envDefault "DBPASS" ""
  db   <- envDefault "DBNAME" "biking"
  host <- envDefault "DBHOST" "localhost"
  return (ParamSettings host 5432 user pass db)     -- <4>

getPool :: IO DB
getPool = do
  pgConfig <- getPgConfig
  acquirePool pgConfig poolConfig                   -- <5>

getConnection :: AppM a -> IO a
getConnection m = getPool >>= runReaderT m          -- <6>
-- end::DBAll[]


-- tag::DBEnvDefault[]
envDefault :: String -> ByteString -> IO ByteString
envDefault var fallback = do
  maybeVar <- lookupEnv var                         -- <1>
  return (maybe fallback pack maybeVar)             -- <2>
-- end::DBEnvDefault[]
