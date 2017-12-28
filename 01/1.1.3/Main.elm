module Main exposing (..)

import Html exposing (text, h1)
import Mouse
import Platform.Cmd exposing (none)
import Platform.Sub exposing (Sub)


type Model
    = Model Mouse.Position


view (Model pos) =
    h1 []
        [ text "x: "
        , pos.x |> toString |> text
        , text " y: "
        , pos.y |> toString |> text
        ]


update msg model =
    ( Model msg, none )



-- tag::mouseSignal[]
main : Program Never Model Mouse.Position
main =
    Html.program
        { init = ( Model { x = 0, y = 0 }, none )
        , update = update
        , subscriptions = mousePositionSubscription
        , view = view
        }


mousePositionSubscription : Model -> Sub Mouse.Position
mousePositionSubscription _ =
    let
        mouseMoveFunction pos =
            pos
    in
        Mouse.moves mouseMoveFunction
-- end::mouseSignal[]
