 {-# LANGUAGE OverloadedStrings #-}

 module Views where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Prelude hiding (head, id, div)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as B
import Text.Digestive.Blaze.Html5
import Text.Digestive.Form
  ( (.:)
  , Form
  , Formlet
  , string
  , stringRead
  , text
  , validate
  )
import Text.Digestive.View (View)
import Text.Digestive.Types (Result(..))
import Web.Spock.Safe (ActionT, html, renderRoute)

import Types (BikeRide(..), Rider(..), formRoute)


html' :: MonadIO m => B.Html -> ActionT m a
html' = html . toStrict . renderHtml


-- tag::ViewsIndex[]
-- import qualified Text.Blaze.Html5 as B                       -- <1>
index :: [BikeRide] -> B.Html
index rides = B.html $ do
  let newRideLink = B.textValue (renderRoute formRoute)         -- <2>

  B.head $
    B.title "Biking!"
  B.body $ do
    B.h1 "Welcome to biking!"
    B.a ! A.href newRideLink $ "New ride"                       -- <3>
    B.hr
    B.ul $
      for_ rides $                                              -- <4>
        B.li . B.toMarkup
-- end::ViewsIndex[]


-- tag::ViewsRideEntry[]
rideEntry :: Monad m => Form B.Html m BikeRide
rideEntry =
  BikeRide <$> (Rider <$> "name" .: text Nothing
                      <*> "email" .: text Nothing)
           <*> "date" .: dateFormlet' "%F" Nothing
           <*> "distance" .: stringRead "distance" Nothing
-- end::ViewsRideEntry[]


-- tag::ViewsRideEntryForm[]
rideEntryForm :: View B.Html -> Text -> B.Html
rideEntryForm v action =                                        -- <1>
  form v action $ do
    B.div $ do
      B.span $ do
        label "name" v "Rider name: "
        inputText "name" v
      B.span $ do
        label "email" v "Email: "
        inputText "email" v
    B.div $ do
      label "date" v "Date (YYYY-MM-DD): "
      inputText "date" v
    B.div $ do
      label "distance" v "Distance (miles): "
      inputText "distance" v
    B.div $
      B.input ! A.type_ "submit"
-- end::ViewsRideEntryForm[]


-- Helpers


dateFormlet' :: Monad m => String -> Formlet B.Html m Day
dateFormlet' fmt d =
  validate (vFunc fmt (B.toMarkup ("invalid date" :: Text)))
           (string $ formatTime defaultTimeLocale fmt <$> d)

vFunc :: String -> B.Html -> String -> Result B.Html Day
vFunc fmt err time =
  maybe (Error err) Success $
    parseTimeM True defaultTimeLocale fmt time
