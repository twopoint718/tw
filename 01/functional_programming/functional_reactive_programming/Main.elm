module Main where

import Graphics.Element exposing (Element, show)
import Mouse
import Signal exposing (map, Signal)
import String exposing (join)

-- tag::mouseSignal[]
main : Signal Element
main = map showMouse Mouse.position

showMouse : (Int, Int) -> Element
showMouse (x, y) = join ", " [toString x, toString y] |> show
-- end::mouseSignal[]
