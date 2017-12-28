module View exposing (view)

import Date.Format exposing (format)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import DatePicker
import Ride exposing (Ride)
import Model exposing (FieldState, FieldValue(..), Index, Msg, Model)


-- tag::ViewView[]
view : Model -> Html Msg
view model =
    H.body [ A.class "container" ]
        [ H.h1 [] [ H.text "Biking!" ]
        , rideTable model                                         -- <1>
        ]
-- end::ViewView[]


-- tag::ViewRideTable[]
rideTable : Model -> Html Msg
rideTable model =
    let
        numRides =
            List.length model.rides

        indexedRides =                                            -- <1>
            List.map2 tableRow model.rides (List.range 0 (numRides - 1))
    in
        H.table [ A.class "table tabled-bordered" ]
            [ H.thead []
                [ H.tr []
                    [ H.th [] []
                    , H.th [] [ H.text "Email" ]
                    , H.th [] [ H.text "Distance" ]
                    , H.th [] [ H.text "Date" ]
                    , H.th [] [ H.text "Action" ]
                    ]
                ]
            , H.tbody [] <| indexedRides ++ rideForm model        -- <2>
            ]
-- end::ViewRideTable[]


-- tag::ViewRideForm[]
rideForm : Model -> List (Html Msg)
rideForm model =
    [ H.tr [ A.style [ ( "height", "80px" ) ] ]
        [ H.td [] []
        , H.td []
            [ H.div
                [ A.class <|
                    "col-lg-10"
                        ++ showInputClass model.userState         -- <1>
                ]
                [ H.input
                    [ A.class "form-control"
                    , A.type_ "email"
                    , A.id "email"
                    , E.onInput Model.UpdateUser                  -- <2>
                    , A.placeholder "Enter email (e.g. usr@example.com)"
                    , A.value model.userState.input               -- <3>
                    ]
                    []
                , H.span
                    [ A.class "help-block"
                    ]
                    [ H.text <| showHelpMessage model.userState ] -- <4>
                ]
            ]
-- end::ViewRideForm[]
        , H.td []
            [ H.div
                [ A.class <|
                    "col-lg-10"
                        ++ showInputClass model.distState
                ]
                [ H.input
                    [ A.class "form-control"
                    , A.type_ "text"
                    , A.id "distance"
                    , E.onInput Model.UpdateDist
                    , A.placeholder "Enter distance in mi. (e.g. 5.3)"
                    , A.value model.distState.input
                    ]
                    []
                , H.span
                    [ A.class "help-block"
                    ]
                    [ H.text <| showHelpMessage model.distState ]
                ]
            ]
        , H.td []
            [ H.div [ A.class "col-lg-10" ]
                [ DatePicker.view model.datePicker
                    |> H.map Model.ToDatePicker
                ]
            ]
        , H.td []
            [ H.button
                [ A.class "btn btn-primary"
                , A.value "Add"
                , E.onClick Model.Add
                ]
                [ H.text "Add" ]
            ]
        ]
    ]


-- tag::ViewTableRow[]
tableRow : Ride -> Index -> Html Msg
tableRow ride index =
    H.tr []
        [ H.td []
            [ H.img [ A.src ride.avatar.url ] [] ]                -- <1>
        , H.td [] [ H.text ride.userEmail.email ]
        , H.td [] [ H.text (toString ride.distance) ]
        , H.td [] [ H.text (format "%b %e, %Y" ride.date) ]
        , H.td []
            [ H.button
                [ A.class "btn btn-secondary"
                , A.value "Remove"
                , E.onClick (Model.Remove index)                  -- <2>
                ]
                [ H.text "Remove" ]
            ]
        ]
-- end::ViewTableRow[]


showInputClass : FieldState a -> String
showInputClass fs =
    case fs.value of
        Good _ ->
            " has-success"

        Neutral ->
            ""

        Problem _ ->
            " has-error"


showHelpMessage : FieldState a -> String
showHelpMessage fs =
    case fs.value of
        Good _ ->
            ""

        Neutral ->
            ""

        Problem infoMsg ->
            infoMsg
