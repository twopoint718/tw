{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Controllers where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Functor.Identity (Identity(Identity))
import Web.Spock.Digestive (runForm)
import Web.Spock.Shared (html, redirect)
import Web.Spock.Safe (renderRoute, root)
import qualified Hasql as H
import qualified Hasql.Postgres as HP

import Types
import Views (html')
import qualified Views


-- tag::ControllersIndex[]
index :: ActionM ()
index = do
  result <- runQuery listRides                           -- <1>
  case result of                                         -- <2>
    Left err -> liftIO (print err)                       -- <3>
    Right rides -> html' (Views.index rides)             -- <4>
-- end::ControllersIndex[]


-- tag::ControllersForm[]
form :: ActionM ()
form = do
  (view, _) <- runForm "add-ride" Views.rideEntry            -- <1>
  html' (Views.rideEntryForm view (renderRoute submitRoute)) -- <2>
-- end::ControllersForm[]


-- tag::ControllersSubmitForm[]
submitForm :: ActionM ()
submitForm = do
  (_, result) <- runForm "add-ride" Views.rideEntry      -- <1>
  case result of
    Nothing -> redirect "form"                           -- <2>
    Just ride -> do
      qResult <- runQuery (saveRide ride)                -- <3>
      case qResult of
        Left err -> do                                   -- <4>
          liftIO (print err)
          html "An error ocurred"
        Right _ -> redirect (renderRoute root)           -- <5>
-- end::ControllersSubmitForm[]


-- Database Interaction (These are private)


listRides :: H.Tx HP.Postgres s [BikeRide]
-- tag::ControllersListRides[]
listRides = do
  let makeRide (name', email', date', distance') =       -- <1>
        BikeRide (Rider name' email') date' distance'
  results <- H.listEx $ [H.stmt|
    SELECT name, email, date, distance
    FROM bike_rides
    JOIN riders ON riders.id = bike_rides.rider_id
    ORDER BY date
  |]                                                     -- <2>
  return $ map makeRide results                          -- <3>
-- end::ControllersListRides[]


saveRide :: BikeRide -> H.Tx HP.Postgres s Int
-- tag::ControllersSaveRide[]
saveRide (BikeRide (Rider name' email') date' distance') = do
  Identity riderId <- H.singleEx $ [H.stmt|
    INSERT INTO riders (name, email)
    VALUES (?, ?)
    RETURNING id
    |] name' email'
  Identity rideId <- H.singleEx $ [H.stmt|
    INSERT INTO bike_rides (rider_id, date, distance)
    VALUES (?, ?, ?)
    RETURNING id
    |] (riderId :: Int) date' distance'
  return rideId
-- end::ControllersSaveRide[]


runQuery :: (forall s. H.Tx HP.Postgres s a)
         -> ActionM (Either (H.SessionError HP.Postgres) a)
runQuery query = do
  pool' <- lift ask
  lift $ H.session pool' $
    H.tx (Just (H.ReadCommitted, Just True)) query
