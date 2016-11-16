module View exposing (..)

import Date.Format exposing (format)
import Html
import Html exposing (..)
import Html.Attributes as A

import Types exposing (Msg (..), Model, User, Ride, FormEntry)
import Forms exposing (riderForm)


-- tag::ViewMain[]
view : Model -> Html Msg
view model =                                      -- <1>
  body [ A.class "container" ]
    [ h1 [] [ text "Biking!" ]
    , rideTable model.rides                       -- <2>
    , div [ A.class "row" ]
        [ div [ A.class "col-md-6" ]
            [ riderForm model                     -- <3>
            ]
        ]
    ]
-- end::ViewMain[]


-- tag::ViewTable[]
rideTable : List Ride -> Html a
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


tableRow : Ride -> Html a
tableRow ride =                                   -- <2>
  tr []
    [ td [] [text ride.user.email]
    , td [] [text (toString ride.distance)]
    , td [] [text (format "%b %e, %Y" ride.date)]
    ]
-- end::ViewTable[]
