module View where

import Date.Format exposing (format)
import Html
import Html exposing (..)
import Html.Attributes as A
import Maybe exposing (withDefault)

import Types exposing (Action (..), Model, User, Ride, FormEntry)
import Forms exposing (riderForm)


-- tag::ViewMain[]
view : Signal.Address Action -> Model -> Html
view address model =                              -- <1>
  body [ A.class "container" ]
    [ h1 [] [ text "Biking!" ]
    , rideTable model.rides                       -- <2>
    , div [ A.class "row" ]
        [ div [ A.class "col-md-6" ]
            [ riderForm address model             -- <3>
            ]
        , div [ A.class "col-md-6" ]
            [ h2 [] [ text "this is the rest" ] ]
        ]
    ]
-- end::ViewMain[]


-- tag::ViewTable[]
rideTable : List Ride -> Html
rideTable rides =
  table [ A.class "table tabled-bordered" ]
  [ thead []
    [ tr []
      [ th [] [text "Name"]
      , th [] [text "Distance"]
      , th [] [text "Date"]
      ]
    ]
  , tbody [] (List.map tableRow rides)            -- <1>
  ]

tableRow : Ride -> Html
tableRow ride =                                   -- <2>
  tr []
    [ td [] [text ride.user.email]
    , td [] [text (toString ride.distance)]
    , td [] [text (format "%b %e, %Y" ride.date)]
    ]
-- end::ViewTable[]
