module Update exposing (update)

import DatePicker
import List.Extra as List
import MD5
import Model exposing (Model, Msg(..), FieldState, FieldValue(..))
import Ride
import Result.Extra as Result
import Debug

-- tag::UpdateFunction[]
update :
    Msg
    -> Model
    -> ( Model, Cmd Msg )
update msg model =
    let
        newState str =
            { input = str, value = Neutral }

        onUpdate =
            validateUser >> validateDist                          -- <1>
    in
        case msg of
            UpdateUser str ->
                ( onUpdate { model | userState = newState str }   -- <2>
                , Cmd.none
                )

            UpdateDist str ->
                ( onUpdate { model | distState = newState str }   -- <2>
                , Cmd.none
                )

            Add ->
                ( addRideIfValid model, Cmd.none )                -- <3>

            Remove i ->
                ( { model | rides = List.removeAt i model.rides } -- <4>
                , Cmd.none
                )

            ToDatePicker msg ->
                handleDatePicker msg model                        -- <5>
-- end::UpdateFunction[]


validateUser : Model -> Model
validateUser model =
    { model
    | userState =
        model.userState.input
            |> validateField Ride.validateEmail
    }


-- tag::UpdateHelpers[]
validateField : (String -> Result String a) -> String -> FieldState a
validateField validate str =
    { input = str
    , value = Result.unpack Problem Good (validate str)           -- <1>
    }


validateDist : Model -> Model                                     -- <2>
validateDist model =
    { model
    | distState =
        model.distState.input
            |> validateField Ride.validateDist
    }


addRideIfValid : Model -> Model                                   -- <3>
addRideIfValid model =
    case Debug.log "curr state: " ( model.userState.value, model.distState.value, model.date ) of
        ( Good email, Good dist, Just date ) ->
            let
                url =
                    Ride.Url <|
                        "http://gravatar.com/avatar/"
                            ++ MD5.hex email.email

                newRide =
                    { userEmail = email
                    , distance = dist
                    , date = date
                    , avatar = url
                    }
            in
                { model
                    | rides = model.rides ++ [ newRide ]
                    , userState = Model.emptyField
                    , distState = Model.emptyField
                    -- , date = Nothing
                }

        _ ->
            model


handleDatePicker : DatePicker.Msg -> Model -> ( Model, Cmd Msg )  -- <4>
handleDatePicker msg model =
    let
        ( datePicker, datePickerCmd, mDate ) =
            DatePicker.update msg model.datePicker

        debouncedDate =
            case mDate of
                Nothing ->
                    model.date

                date ->
                    date
    in
        { model
            | date = Debug.log "picked date" debouncedDate
            , datePicker = datePicker
        }
            ! [ Cmd.map ToDatePicker datePickerCmd ]
-- end::UpdateHelpers[]
