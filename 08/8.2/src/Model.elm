module Model
    exposing
        ( FieldState
        , FieldValue(..)
        , Index
        , Msg(..)
        , Model
        , emptyField
        , initialModel
        )

import Date exposing (Date)
import DatePicker exposing (defaultSettings)
import Ride
    exposing
        ( Ride
        , Email
        )


type alias Index =
    Int


-- tag::ModelModel[]
type FieldValue a                                     -- <1>
    = Good a
    | Neutral
    | Problem String


type alias FieldState a =                             -- <1>
    { input : String
    , value : FieldValue a
    }


type alias Model =                                    -- <2>
    { rides : List Ride
    , datePicker : DatePicker.DatePicker
    , date : Maybe Date
    , distState : FieldState Float
    , userState : FieldState Email
    }
-- end::ModelModel[]


-- tag::ModelMsg[]
type Msg
    = UpdateDist String                               -- <1>
    | UpdateUser String                               -- <1>
    | ToDatePicker DatePicker.Msg                     -- <2>
    | Add                                             -- <3>
    | Remove Index                                    -- <4>
-- end::ModelMsg[]


-- tag::ModelInitialModel[]
initialModel : ( Model, Cmd Msg )
initialModel =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init
                { defaultSettings | isDisabled = always False }   -- <1>
    in
        { rides = []                                              -- <2>
        , date = Nothing
        , datePicker = datePicker                                 -- <3>
        , userState = emptyField                                  -- <4>
        , distState = emptyField                                  -- <4>
        }
            ! [ Cmd.map ToDatePicker datePickerCmd ]              -- <5>
-- end::ModelInitialModel[]


emptyField : FieldState a
emptyField =
    { input = "", value = Neutral }
