module Main exposing (main)

import Html as Html
import Model exposing (Msg, Model, initialModel)
import View exposing (view)
import Update exposing (update)


subscriptions : Model -> Sub a
subscriptions _ =
    Sub.none                                          -- <1>


main : Program Never Model Msg
main =
    Html.program
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions               -- <2>
        }
