--------------------------------------------------------------------------------
-- AUTOGENERATED CODE. DO NOT MODIFY WITHOUT A GOOD REASON, BUDDY.
--------------------------------------------------------------------------------
module Ride exposing (..)


import Date exposing (Date)
import Json.Decode exposing (..)


date : Decoder Date.Date
date =
    flip andThen string <| \s ->
        case Date.fromString s of
            Ok date_ -> succeed date_
            Err msg -> fail msg


(<*>) : Decoder (a -> b) -> Decoder a -> Decoder b
(<*>) = flip (map2 (|>))


type alias Ride =
    { id : Int
    , user : String
    , date : Date
    , distance : Float
    }


ride : Decoder Ride
ride = succeed Ride
    <*> field "id" int
    <*> field "user" string
    <*> field "date" date
    <*> field "distance" float


--------------------------------------------------------------------------------
