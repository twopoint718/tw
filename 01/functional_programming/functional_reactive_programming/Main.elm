import Graphics.Element (Element)
import Mouse
import Signal (map, Signal)
import String (join)
import Text (asText)

-- tag::mouseSignal[]
main : Signal Element
main = map showMouse Mouse.position

showMouse : (Int, Int) -> Element
showMouse (x, y) = join ", " [toString x, toString y] |> asText
-- end::mouseSignal[]
