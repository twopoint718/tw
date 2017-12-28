{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Ride where


import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ReaderT, asks)
import           Control.Monad.Trans.Class   (lift)
import           Data.Proxy
import qualified Data.Text                   as Text
import           Data.Time                   (Day)
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics                (Generic)

import           Config
import           ToDecoder                   (ToDecoder, g_ToDecoder, toDecoder)
import           ToElm                       (ToElm, g_ToElm, toElm)

-- tag::RideDatabase[]
-- <1>
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Ride json
    user Text.Text
    date Day
    distance Double
    deriving Show Generic
|]


doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll                -- <2>


runDb :: ReaderT SqlBackend IO a -> AppActionM a
runDb query = do
    pool <- lift $ asks configPool                    -- <3>
    liftIO $ runSqlPool query pool
-- end::RideDatabase[]


{- oops: https://github.com/yesodweb/persistent/issues/578 ! It looks like I
    can't have DeriveAnyClass (to be able to derive ToElm) turned on and at the
    same time use Persistent. So, this is necessary:
-}
-- tag::RideGeneratedElmCode[]
instance ToElm Ride where
    toElm _ = g_ToElm (Proxy :: Proxy Ride)


instance ToDecoder Ride where
    toDecoder _ = g_ToDecoder (Proxy :: Proxy Ride)
-- end::RideGeneratedElmCode[]
