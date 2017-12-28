module Ride exposing (Ride, Email, Url, validateDist, validateEmail)

import Date exposing (Date, fromString, fromTime)
import Regex


-- tag::RideModel[]
type alias Email =                                    -- <1>
    { email : String }


type alias Url =                                      -- <1>
    { url : String }


type alias Ride =                                     -- <2>
    { userEmail : Email
    , distance : Float
    , date : Date
    , avatar : Url
    }
-- end::RideModel[]


validateEmail : String -> Result String Email
validateEmail user =
    case Regex.contains (Regex.regex "@") user of
        True ->
            Ok { email = user }

        False ->
            Err "That does not look like a user email!"


validateDist : String -> Result String Float
validateDist =
    String.toFloat
