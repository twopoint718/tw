{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Config where


import           Control.Exception                    (throwIO)
import           Control.Monad.Logger                 (LogLevel (..),
                                                       runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Reader                 (ReaderT)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8                as BS
import           Data.Monoid                          ((<>))
import           Data.Text.Lazy                       (Text)
import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           System.Environment                   (lookupEnv)
import           Web.Scotty.Trans                     (ActionT, ScottyT)


type AppM a = ScottyT Text (ReaderT Config IO) a


type AppActionM a = ActionT Text (ReaderT Config IO) a


data Config
    = Config
    { configPool   :: ConnectionPool
    , configEnv    :: Environment
    , configLogEnv :: LogLevel
    }


data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)


makePool :: Environment -> IO ConnectionPool
makePool Test = runNoLoggingT (createPostgresqlPool (connStr "_test") (envPool Test))
makePool Development = runStdoutLoggingT (createPostgresqlPool (connStr "") (envPool Development))
makePool Production = do
    pool <- runMaybeT $ do
        let keys = [ "host=", "port=", "user=", "password=", "dbname=" ]
            envs = [ "PGHOST", "PGPORT", "PGUSER", "PGPASS", "PGDATABASE" ]
        envVars <- traverse (MaybeT . lookupEnv) envs
        let prodStr = BS.pack $ unwords $ zipWith (<>) keys envVars
        runStdoutLoggingT $ createPostgresqlPool prodStr (envPool Production)
    case pool of
        Nothing    -> throwIO (userError "DB Config not specified")
        Just pool' -> return pool'


connStr :: BS.ByteString -> ConnectionString
connStr suffix = "host=localhost dbname=generic-ride" <> suffix <> " user=chris port=5432"


envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8


setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout


logLevel :: Environment -> LogLevel
logLevel Development = LevelInfo
logLevel Test        = LevelDebug
logLevel Production  = LevelError
