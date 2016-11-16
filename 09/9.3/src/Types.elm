module Types exposing (..)

import Date exposing (Date, fromString, fromTime)
import DatePicker

-- tag::Model[]
type alias Model =
  { rides : List Ride                  -- <1>
  , distField : FormEntry Float        -- <2>
  , userField : FormEntry User         -- <2>
  , datePicker : DatePicker.DatePicker -- <3>
  , date : Maybe Date
  }


type alias Ride =                      -- <4>
  { user : User
  , distance : Float
  , date : Date
  }


type alias User =                      -- <5>
  { email : String }
-- end::Model[]


-- tag::FormState[]
type FormState a                            -- <1>
  = FormError String
  | Value a
  | Neutral


type alias FormEntry a =
  { decoder : String -> Result String a     -- <2>
  , encoder : a -> String
  , err : String -> ErrorMsg
  , help : String
  , label : String
  , updater : String -> UpdateMsg           -- <3>
  , userInput : String
  , state : FormState a                     -- <4>
  }
-- end::FormState[]


-- tag::Messages[]
type ErrorMsg
  = ErrorDist String
  | ErrorUser String


type UpdateMsg
  = UpdateDist String
  | UpdateUser String
-- end::Messages[]


-- tag::Action[]
type Msg
  = Error ErrorMsg              -- <1>
  | Update UpdateMsg            -- <2>
  | Submit                      -- <3>
  | ToDatePicker DatePicker.Msg -- <4>
-- end::Action[]
