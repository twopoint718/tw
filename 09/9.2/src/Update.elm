module Update exposing (..)

import DatePicker
import Maybe
import Result.Extra as Result

import Model exposing (clearField)
import Types exposing
  ( Msg (..)
  , ErrorMsg (..)
  , FormEntry
  , FormState (..)
  , Model
  , Ride
  , UpdateMsg (..)
  , User
  )


-- tag::UpdateFunction[]
update : Msg -> Model -> (Model, Cmd Msg)                  -- <1>
update msg model =
  case msg of
    Error err ->
      handleError err model

    Update update ->
      handleUpdate update model

    Submit ->
      handleSubmit model

    ToDatePicker msg ->                                    -- <2>
      handleDatePicker msg model
-- end::UpdateFunction[]


-- tag::HandlerFunctions[]
handleError : ErrorMsg -> Model -> (Model, Cmd Msg)
handleError err model =                                        -- <1>
  newModel <| case err of
    ErrorDist m ->
      { model | distField = errorFn model.distField m }
    ErrorUser m ->
      { model | userField = errorFn model.userField m }


handleUpdate : UpdateMsg -> Model -> (Model, Cmd Msg)
handleUpdate update model =                                    -- <2>
  newModel <| case update of
    UpdateDist n ->
      { model | distField = updateFn model.distField n }
    UpdateUser u ->
      { model | userField = updateFn model.userField u }


handleSubmit : Model -> (Model, Cmd Msg)
handleSubmit model =                                           -- <3>
  Model.readRide model                                         -- <4>
    |> Maybe.map (Model.addRide model)                         -- <5>
    |> Maybe.withDefault model                                 -- <6>
    |> clearForm                                               -- <7>
    |> newModel                                                -- <7>


handleDatePicker : DatePicker.Msg -> Model -> (Model, Cmd Msg)
handleDatePicker msg model =                                   -- <8>
  let
    (datePicker, datePickerFx, mDate) =                        -- <9>
      DatePicker.update msg model.datePicker

    date =                                                     -- <10>
      case mDate of
        Nothing ->
          model.date

        date ->
          date
  in
    { model                                                    -- <11>
        | date = date
        , datePicker = datePicker
    }
      ! [Cmd.map ToDatePicker datePickerFx]
-- end::HandlerFunctions[]


-----------------------------------------------------------
-- Helpers


-- tag::UpdateHelpers[]
clearForm : Model -> Model
clearForm model =                                    -- <1>
  { model
      | distField = clearField model.distField
      , userField = clearField model.userField
      , date = Nothing
  }


errorFn : FormEntry a -> String -> FormEntry a
errorFn form errStr =
  { form | state = FormError errStr }                -- <2>


updateFn : FormEntry a -> String -> FormEntry a
updateFn form input =
  let
    newState =
      input
        |> form.decoder
        |> Result.unpack FormError Value
  in
    { form | state = newState, userInput = input }   -- <3>


toMaybe : FormState a -> Maybe a
toMaybe fs =
  case fs of                                         -- <4>
    Neutral -> Nothing
    FormError _ -> Nothing
    Value a -> Just a


newModel : Model -> (Model, Cmd Msg)                 -- <5>
newModel = flip (,) Cmd.none
-- end::UpdateHelpers[]
