{-# LANGUAGE OverloadedStrings #-}

module Web (app) where


import           Control.Monad.Reader          (ReaderT, runReaderT)
import           Data.Int                      (Int64)
import           Data.Semigroup                ((<>))
import qualified Database.Persist              as DB
import qualified Database.Persist.Postgresql   as DB
import           Network.HTTP.Types.Status     (Status (..))
import           Network.Wai                   (Application)
import           Network.Wai.Middleware.Static (addBase, noDots, staticPolicy)
import           Ride                          (Ride, runDb)
import           Web.Scotty.Trans              (file, get, json, middleware,
                                                param, scottyAppT, setHeader,
                                                status)

import           Config


app :: Config -> IO Application
app cfg = scottyAppT (onAction cfg) routes


onAction :: Config -> ReaderT Config m a -> m a
onAction cfg resp = runReaderT resp cfg


-- tag::WebApplication[]
routes :: AppM ()
routes = do
    middleware $
        staticPolicy (noDots <> addBase "assets")     -- <1>

    -- User-facing
    get "/" $ do                                      -- <2>
        setHeader "Content-Type" "text/html"
        file "assets/html/elm.html"

    -- API
    get "/rides" $ do                                 -- <3>
        rides <- allRides
        json rides

    get "/rides/:rideId" $ do                         -- <4>
        i <- param "rideId"
        mride <- getRide i
        case mride of
            Nothing -> status (Status 404 "not found")
            Just r  -> json r
-- end::WebApplication[]


--------------------------------------------------------------------------------
-- Rides DB API


-- tag::WebDatabase[]
allRides :: AppActionM [DB.Entity Ride]
allRides = runDb (DB.selectList [] [])                -- <1>


getRide :: Int64 -> AppActionM (Maybe Ride)
getRide i = runDb (DB.get (DB.toSqlKey i))            -- <2>
-- end::WebDatabase[]
