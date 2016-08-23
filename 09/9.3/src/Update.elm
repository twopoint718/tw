module Update where

import Date exposing (Date)
import Effects exposing (Effects)
import Maybe exposing (andThen, withDefault)

import Types exposing
  ( Action (..)
  , ErrorMsg (..)
  , FormEntry
  , Model
  , Ride
  , SetMsg (..)
  , UpdateMsg (..)
  , User
  )


-- tag::UpdateFunction[]
update : Action -> Model -> (Model, Effects Action)         -- <1>
update action model =
  let
    handleError err =                                       -- <2>
      newModel <| case err of                               -- <3>
        ErrorDate m ->
          { model | dateField = errorFn model.dateField m } -- <4>
        ErrorDist m ->
          { model | distField = errorFn model.distField m }
        ErrorUser m ->
          { model | userField = errorFn model.userField m }

    handleSet set =
      newModel <| case set of
        SetDate d ->
          { model | dateField = setFn model.dateField d }   -- <5>
        SetDist n ->
          { model | distField = setFn model.distField n }
        SetUser u ->
          { model | userField = setFn model.userField u }

    handleUpdate update =                                   -- <6>
      newModel <| case update of
        UpdateDate d ->
          { model | dateField = updateFn model.dateField d }
        UpdateDist n ->
          { model | distField = updateFn model.distField n }
        UpdateUser u ->
          { model | userField = updateFn model.userField u }

    handleSubmit =                                            -- <7>
      let
        addRide : Ride -> Maybe Model
        addRide r = Just { model | rides = r :: model.rides } -- <8>

        makeRide : Float -> Date -> User -> Ride
        makeRide dist date user =                             -- <9>
          { distance = dist, date = date, user = user }

        maybeRide : Maybe Ride
        maybeRide = Maybe.map3 makeRide                       -- <10>
          model.distField.value
          model.dateField.value
          model.userField.value

        model' : Maybe Model
        model' = maybeRide `andThen` addRide                  -- <11>
      in
        newModel (withDefault model model')                   -- <12>
  in
    case action of                                            -- <13>
      Error err -> handleError err
      Set set -> handleSet set
      Update update -> handleUpdate update
      Submit -> handleSubmit
-- end::UpdateFunction[]


-----------------------------------------------------------
-- Helpers


-- tag::UpdateHelpers[]
errorFn : FormEntry a -> String -> FormEntry a
errorFn form errStr =
  { form | formError = Just errStr }                -- <1>


setFn : FormEntry a -> a -> FormEntry a
setFn form val =
  { form | formError = Nothing, value = Just val }  -- <2>


updateFn : FormEntry a -> String -> FormEntry a
updateFn form input =
  { form | formError = Nothing, userInput = input } -- <3>
-- end::UpdateHelpers[]


-- tag::UpdateNewModel[]
newModel : Model -> (Model, Effects Action)
newModel = flip (,) Effects.none
-- end::UpdateNewModel[]
