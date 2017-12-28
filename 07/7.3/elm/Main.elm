module Main exposing (..)


import Html as H
import Html.Attributes as A
import Http as Http
import Json.Decode exposing (list)
import Date exposing (Date)
import Formatting exposing (Format, (<>), any, s, string, print, int, html, float)
import Task


import Ride exposing (Ride)


--------------------------------------------------------------------------------
-- SERVER COMMUNICATION API


handleError : (a -> Msg) -> Result Http.Error a -> Msg
handleError f result = case result of
    Ok val -> f val
    Err err -> ServerError (toString err)


-- tag::MainElmServerCommunication[]
getRideList : Cmd Msg
getRideList =
    Http.send (handleError RideList) <|               -- <1>
        Http.get "rides/" (list Ride.ride)


getRide : Int -> Cmd Msg
getRide rideId =
    Http.send (handleError SingleRide) <|             -- <2>
        Http.get ("rides/" ++ toString rideId) Ride.ride
-- end::MainElmServerCommunication[]


--------------------------------------------------------------------------------
-- DATATYPES


-- tag::MainElmDatatypes[]
type alias Model =
    { rides : List Ride
    , errors : Maybe String
    }


type Msg
    = RideList (List Ride)
    | SingleRide Ride
    | ServerError String
-- end::MainElmDatatypes[]


--------------------------------------------------------------------------------
-- MAIN


-- tag::MainElmMain[]
main : Program Never Model Msg
main = H.program
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }


init : (Model, Cmd Msg)
init = (initialModel, getRideList)                    -- <1>


initialModel : Model
initialModel =
    { rides = []
    , errors = Nothing
    }
-- end::MainElmMain[]


--------------------------------------------------------------------------------
-- UPDATE


-- tag::MainElmUpdate[]
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    RideList rides ->                                  
        ( { model                                     -- <1>
            | rides = rides
            , errors = Nothing }
        , Cmd.none
        )
    SingleRide ride ->
        ( { model                                     -- <2>
            | rides = [ride]
            , errors = Nothing }
        , Cmd.none
        )
    ServerError e ->
        ( { model                                     -- <3>
            | errors = Just ("Server: " ++ String.left 20 e ++ "...") }
        , Cmd.none
        )
-- end::MainElmUpdate[]


--------------------------------------------------------------------------------
-- VIEW


view : Model -> H.Html Msg
view { rides, errors } =
    let errorMessages = case errors of
            Nothing -> []
            Just msgs -> [ H.div [errorStyle] [H.text msgs] ]
    in  H.main_ [pageStyle] <| errorMessages ++
            [ H.h1 [] [H.text "Rides"]
            , makeTable rides
            ]


makeTable : List Ride -> H.Html a
makeTable rides =
    H.table [ A.style [("border-collapse", "collapse")] ] <|
        [ H.tr []
            [ H.th [headerStyle, cellStyle] [ H.text "User"]
            , H.th [headerStyle, cellStyle] [ H.text "Date"]
            , H.th [headerStyle, cellStyle] [ H.text "Distance" ]
            , H.th [headerStyle, cellStyle] [ H.text "Actions" ]
            ]
        ] ++
        List.map2 makeTableRow
            rides
            (cycle (List.length rides) [shadedRow, unShadedRow])



makeTableRow : Ride -> H.Attribute a -> H.Html a
makeTableRow ride shade =
    let attrs = [cellStyle, shade]
    in  H.tr []
            [ H.td attrs [H.text ride.user]
            , H.td attrs [H.text (humanDate ride.date)]
            , H.td attrs [H.text (toString ride.distance)]
            , H.td attrs
                [H.a [A.href ("/rides/" ++ toString ride.id)] [H.text "View"]]
            ]


--------------------------------------------------------------------------------
-- STYLES


pageStyle : H.Attribute a
pageStyle = A.style
    [ ("width", "980px")
    , ("margin-right", "auto")
    , ("margin-left", "auto")
    , ("font-family", "Helvetica, sans-serif")
    ]


errorStyle : H.Attribute a
errorStyle = A.style
    [ ("background", "firebrick")
    , ("color", "white")
    , ("padding", "10px")
    ]


headerStyle : H.Attribute a
headerStyle = A.style [("border-bottom","1px solid black")]

cellStyle : H.Attribute a
cellStyle = A.style
    [ ("padding", "10px 10px")
    , ("text-align", "center")
    ]

shadedRow : H.Attribute a
shadedRow = A.style [("background-color", "#fff6e5")]


unShadedRow : H.Attribute a
unShadedRow = A.style []



--------------------------------------------------------------------------------
-- HELPER FUNCTIONS


humanDate : Date -> String
humanDate date =
    let dateTmpl = any <> s " " <> int <> s ", " <> int
    in  print dateTmpl (Date.month date) (Date.day date) (Date.year date)


-- cycle 5 [1, 2] = [1, 2, 1, 2, 1]
cycle : Int -> List a -> List a
cycle n list =
    let go : Int -> List a -> List a
        go i tmp = case (i, tmp) of
            (0, _) -> []               -- done
            (n, []) -> go n list -- refresh the temp list
            (n, x::xs) -> x :: go (n - 1) xs
    in  go n list


monthToInt : Date.Month -> Int
monthToInt m = case m of
    Date.Jan -> 1
    Date.Feb -> 2
    Date.Mar -> 3
    Date.Apr -> 4
    Date.May -> 5
    Date.Jun -> 6
    Date.Jul -> 7
    Date.Aug -> 8
    Date.Sep -> 9
    Date.Oct -> 10
    Date.Nov -> 11
    Date.Dec -> 12
