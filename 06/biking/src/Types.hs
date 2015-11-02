{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE DataKinds #-}

module Types where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Text as T
import Data.Time.Calendar
import Hasql (Pool)
import Hasql.Postgres (Postgres)
import Text.Blaze.Html5 as B
import Web.Spock (ActionT)
import Web.Spock.Safe (Path)


type DB = Pool Postgres
type AppM = ReaderT DB IO
type ActionM = ActionT (ReaderT DB IO)


-- tag::TypesRoutes[]
formR :: Path '[]
formR = "form"


submitR :: Path '[]
submitR = "submit"
-- end::TypesRoutes[]


-- tag::TypesDomainTypes[]
data Rider = Rider
  { name :: T.Text
  , email :: T.Text
  } deriving (Show)


data BikeRide = BikeRide
  { rider :: Rider         -- <1>
  , date :: Day
  , distance :: Float
  } deriving (Show)
-- end::TypesDomainTypes[]


instance B.ToMarkup Rider where
  toMarkup = B.toMarkup . name


instance B.ToMarkup BikeRide where
  toMarkup bikeRide = do
    B.b (B.toMarkup $ rider bikeRide)
    " rode "
    B.toMarkup (distance bikeRide)
    " mi. on "
    B.toMarkup (showGregorian $ date bikeRide)
