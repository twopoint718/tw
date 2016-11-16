module Forms exposing (..)

import DatePicker exposing (DatePicker)
import Html exposing (..)
import Html.App as Html
import Html.Attributes as A
import Html.Events as E
import Html.Events exposing (defaultOptions)
import String

import Types exposing
  ( Msg (..)
  , ErrorMsg (..)
  , FormEntry
  , FormState (..)
  , Model
  , User
  )


-- tag::FormsRiderForm[]
riderForm : Model -> Html Msg
riderForm model =
  form
    [ E.onSubmit Submit                                      -- <1>
    , A.action "javascript:void(0);"
    ]
    [ subForm model.distField                                -- <2>
    , subForm model.userField
    , showDatePicker model
    , submit
    ]
-- end::FormsRiderForm[]

-- tag::FormsSubForm[]
subForm : FormEntry a -> Html Msg
subForm ent =
  let
    showHelp =                                               -- <1>
      case ent.state of
        FormError err -> err
        _ -> ent.help
  in
    div [ A.class "row" ]
      [ div
          [ A.class <| String.join " "                       -- <2>
              [ "col-md-6"
              , showFormState ent.state
              ]
          ]
          [ label
              [ A.class "control-label"
              , A.for ent.label                              -- <3>
              ] [ text ent.label ]
          , input
              [ A.class "form-control"
              , A.type' "text"
              , A.id ent.label
              , E.onInput (Update << ent.updater)
              , A.value ent.userInput
              ] []
          , span [ A.id "helpBlock2", A.class "help-block" ]
              [ text showHelp ]                              -- <4>
          ]
      ]
-- end::FormsSubForm[]


submit : Html a
submit =
  button
    [ A.class "btn btn-primary"
    , A.value "Submit"
    ] [ text "Submit" ]


-- tag::FormsShowDatePicker[]
showDatePicker : Model -> Html Msg
showDatePicker model =
  div [ A.class "row" ]
    [ div [ A.class <| String.join " "
              [ "col-md-6"
              , maybe "" (always "has-success") model.date
              ]
          ]
        [ label [ A.class "control-label" ] [ text "date" ]
        , DatePicker.view model.datePicker                   -- <1>
            |> Html.map ToDatePicker                         -- <2>
        ]
    ]
-- end::FormsShowDatePicker[]


-----------------------------------------------------------
-- Helpers


showFormState : FormState a -> String
showFormState val =
  case val of
    FormError _ -> "has-error"
    Value _ -> "has-success"
    Neutral -> ""

maybe : b -> (a -> b) -> Maybe a -> b
maybe default func mVal =
  case mVal of
    Just val -> func val
    Nothing -> default
