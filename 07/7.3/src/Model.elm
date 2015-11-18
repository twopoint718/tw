module Model where

import Date exposing (Date)
import Regex exposing (regex)
import String

import Types exposing
  ( Action (..)
  , ErrorMsg (..)
  , FormEntry
  , Model
  , Ride
  , SetMsg (..)
  , User
  )

-- tag::Model[]
model : Model
model =
  { users = [ { email = "chris@example.com" } ]  -- <1>
  , rides =
      [ { user = { email = "chris@example.com" } -- <2>
        , distance = 5.2
        , date = makeTime "2014-06-10"
        }
      , { user = { email = "sarah@example.com" }
        , distance = 5.2
        , date = makeTime "2014-06-10"
        }
      ]
  , distField = defaultField                     -- <3>
      { decoder = String.toFloat                 -- <4>
      , errorFun = ErrorDist                     -- <5>
      , label = "distance"                       -- <6>
      , help = "Enter a number"                  -- <7>
      , setter = SetDist                         -- <8>
      }
-- end::Model[]
  , userField = defaultField
      { decoder = decodeUser
      , errorFun = ErrorUser
      , label = "user"
      , help = "Fill in an email address"
      , setter = SetUser
      }
  , dateField = defaultField
      { decoder = Date.fromString
      , errorFun = ErrorDate
      , label = "date"
      , help = "Enter a date, e.g. 2015-12-20"
      , setter = SetDate
      }
  }


-- tag::FieldOptions[]
type alias FieldOpts a =                         -- <1>
  { decoder : String -> Result String a
  , errorFun : String -> ErrorMsg
  , help : String
  , label : String
  , setter : a -> SetMsg
  }

defaultField : FieldOpts a -> FormEntry a
defaultField opts =                              -- <2>
  { decoder = opts.decoder
  , encoder = toString                           -- <3>
  , err = opts.errorFun
  , formError = Nothing                          -- <4>
  , help = opts.help
  , label = opts.label
  , setter = opts.setter
  , userInput = ""                               -- <5>
  , value = Nothing                              -- <6>
  }
-- end::FieldOptions[]


-- tag::Decoder[]
decodeUser : String -> Result String User
decodeUser user =
  case Regex.contains (regex "@") user of
    True ->
      Ok { email = user }                         -- <1>
    False ->
      Err "That does not look like a user email!" -- <2>
-- end::Decoder[]


makeTime : String -> Date
makeTime str =
  case Date.fromString str of
    Ok date -> date
    Err _ -> Date.fromTime 1447213112000 -- invalid date default
