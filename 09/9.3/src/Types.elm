module Types exposing (..)

import Date exposing (Date, fromString, fromTime)

-- tag::Model[]
type alias Model =
  { users : List User           -- <1>
  , rides : List Ride           -- <2>
  , distField : FormEntry Float -- <3>
  , userField : FormEntry User  -- <3>
  , dateField : FormEntry Date  -- <3>
  }

type alias Ride =               -- <4>
  { user : User
  , distance : Float
  , date : Date
  }

type alias User =
  { email : String }
-- end::Model[]


-- TODO: Maybe unify value/formError (they should not both be Just...)
-- Works? type FormState a = Error String | Help String | Value a
type alias FormEntry a =
  { decoder : String -> Result String a
  , encoder : a -> String
  , err : String -> ErrorMsg
  , formError : Maybe String
  , help : String
  , label : String
  , setter : a -> SetMsg
  , userInput : String
  , value : Maybe a
  }


-- tag::Messages[]
type ErrorMsg
  = ErrorDate String
  | ErrorDist String
  | ErrorUser String

type SetMsg
  = SetDist Float
  | SetUser User
  | SetDate Date

type UpdateMsg
  = UpdateDist String
  | UpdateDate String
  | UpdateUser String
-- end::Messages[]


-- tag::Action[]
type Msg
  = Error ErrorMsg   -- <1>
  | Set SetMsg       -- <2>
  | Submit           -- <3>
  | Update UpdateMsg -- <4>
-- end::Action[]
