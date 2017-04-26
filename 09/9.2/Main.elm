module Main exposing (..)

import Html as Html

-- tag::MainFunction[]
import Model exposing (model)
import View exposing (view)
import Update exposing (update)
import Types exposing (Model, Msg)


subscriptions : Model -> Sub a
subscriptions _ = Sub.none                                     -- <1>


main : Program Never Model Msg
main =
    Html.program
      { init = model
      , view = view
      , update = update
      , subscriptions = subscriptions                          -- <2>
      }
-- end::MainFunction[]
