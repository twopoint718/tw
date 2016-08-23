module Forms exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import Html.Events exposing (defaultOptions)
import Result.Extra exposing (mapBoth)
import String

import Types exposing
  ( Msg (..)
  , ErrorMsg (..)
  , FormEntry
  , Model
  , User
  )


-- tag::FormsRiderForm[]
riderForm : Model -> Html Msg
riderForm model =
  form
    [ E.onSubmit Submit               -- <1>
    , A.action "javascript:void(0);"
    ]
    [ subForm model.distField         -- <2>
    , subForm model.userField
    , subForm model.dateField
    , submit
    ]
-- end::FormsRiderForm[]

-- tag::FormsSubForm[]
subForm : FormEntry a -> Html Msg
subForm ent =
  let
    onError = Error << ent.err                               -- <1>
    onOk = Set << ent.setter

    inputHandler : String -> Msg
    inputHandler str = str                                   -- <2>
      |> ent.decoder                                         -- <3>
      |> mapBoth onError onOk                                -- <4>

    showHelp = Maybe.withDefault ent.help ent.formError      -- <6>
  in
    div [ A.class "row" ]
      [ div
          [ A.class <| String.join " "                       -- <7>
              [ "col-md-6"
              , showError ent.formError
              , showOkay ent.value
              ]
          ]
          [ label
              [ A.class "control-label"
              , A.for ent.label                              -- <8>
              ] [ text ent.label ]
          , input
              [ A.class "form-control"
              , A.type' "text"
              , E.onInput inputHandler
              ] []
          , span [ A.id "helpBlock2", A.class "help-block" ]        
              [ text showHelp ]                              -- <9>
          ]
      ]
-- end::FormsSubForm[]


submit : Html a
submit =
  button
    [ A.class "btn btn-primary"
    , A.value "Submit"
    ] [ text "Submit" ]


-----------------------------------------------------------
-- Helpers


showError : Maybe a -> String
showError err =
  case err of
    Just _ -> "has-error"
    Nothing -> ""


showOkay : Maybe a -> String
showOkay val =
  case val of
    Just val' -> "has-success"
    Nothing -> ""
