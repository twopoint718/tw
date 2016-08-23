module Main where

import Html exposing (Html)
import StartApp
import Effects exposing (Effects)

-- tag::MainFunction[]
import Model exposing (model)
import View exposing (view)
import Update exposing (update)
import Types exposing (Model, Action)

init : (Model, Effects Action)
init =
  ( model                      -- <1>
  , Effects.none               
  )

app : StartApp.App Model
app =
    StartApp.start
      { init = init            -- <2>
      , view = view            -- <3>
      , update = update        -- <4>
      , inputs = []
      }

main : Signal Html
main =
  app.html                     -- <5>
-- end::MainFunction[]
