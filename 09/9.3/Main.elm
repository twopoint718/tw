module Main exposing (..)

import Html exposing (Html)
import Html.App as Html

-- tag::MainFunction[]
import Model exposing (model)
import View exposing (view)
import Update exposing (update)
import Types exposing (Model, Msg)

init : (Model, Cmd Msg)
init =
  ( model                      -- <1>
  , Cmd.none
  )


subscriptions : Model -> Sub a
subscriptions _ = Sub.none


main : Program Never
main =
    Html.program
      { init = init            -- <2>
      , view = view            -- <3>
      , update = update        -- <4>
      , subscriptions = subscriptions
      }
-- end::MainFunction[]
