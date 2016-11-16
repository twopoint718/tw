module Model exposing (..)

import Date exposing (Date)
import DatePicker exposing (defaultSettings)
import Maybe exposing (map3)
import Regex exposing (regex)
import Result exposing (map)
import String

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

-- tag::Model[]
model : (Model, Cmd Msg)
model =
  let
    (datePicker, datePickerFx) =                         -- <1>
      DatePicker.init
        { defaultSettings | isDisabled = always False }
  in
    { rides = []                                         -- <2>
    , distField = defaultField                           -- <3>
        { decoder = String.toFloat                       -- <4>
        , errorFun = ErrorDist                           -- <5>
        , label = "distance"                             -- <6>
        , help = "Enter a number"                        -- <7>
        , updater = UpdateDist                           -- <8>
        }
    , userField = defaultField
        { decoder = decodeUser
        , errorFun = ErrorUser
        , label = "user"
        , help = "Fill in an email address"
        , updater = UpdateUser
        }
    , date = Nothing                                     -- <9>
    , datePicker = datePicker                            -- <9>
    }
      ! [Cmd.map ToDatePicker datePickerFx]
-- end::Model[]


-- tag::FieldOptions[]
type alias FieldOpts a =                                 -- <1>
  { decoder : String -> Result String a
  , errorFun : String -> ErrorMsg
  , help : String
  , label : String
  , updater : String -> UpdateMsg
  }

defaultField : FieldOpts a -> FormEntry a
defaultField opts =                                      -- <2>
  { decoder = opts.decoder
  , encoder = toString                                   -- <3>
  , err = opts.errorFun
  , help = opts.help
  , label = opts.label
  , updater = opts.updater
  , userInput = ""                                       -- <4>
  , state = Neutral                                      -- <5>
  }
-- end::FieldOptions[]


-- tag::Decoder[]
decodeUser : String -> Result String User
decodeUser user =
  case Regex.contains (regex "@") user of
    True ->
      Ok { email = user }                                -- <1>
    False ->
      Err "That does not look like a user email!"        -- <2>
-- end::Decoder[]


-- tag::ModelProcessingFunctions[]
addRide : Model -> Ride -> Model
addRide model ride =                                     -- <1>
  { model | rides = ride :: model.rides }


clearField : FormEntry a -> FormEntry a
clearField formEntry =                                   -- <2>
  { formEntry | userInput = "", state = Neutral }


makeRide : Float -> Date -> User -> Ride
makeRide dist date user =                                -- <3>
  { distance = dist, date = date, user = user }


readRide : Model -> Maybe Ride
readRide model = map3 makeRide                           -- <4>
  (model.distField.state |> toMaybe)
  model.date
  (model.userField.state |> toMaybe)
-- end::ModelProcessingFunctions[]


makeTime : String -> Date
makeTime str =
  case Date.fromString str of
    Ok date -> date
    Err _ -> Date.fromTime 1447213112000 -- invalid date default


toMaybe : FormState a -> Maybe a
toMaybe fs =
  case fs of
    Neutral ->
      Nothing

    FormError _ ->
      Nothing

    Value a ->
      Just a
