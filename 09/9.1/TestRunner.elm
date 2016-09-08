port module Main exposing (..)


import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)

import Tests


--main : Program Never
main =
  run emit Tests.suite


port emit : (String, Value) -> Cmd msg
