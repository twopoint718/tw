module Functions exposing (..)

import Color exposing (blue)
import Element exposing (Element, leftAligned)
import Text as T


-- tag::FunctionsArity[]
three : Int -> Int -> Int -> Int -- <1>
three x y z = (x + y) // z

two   : Int -> Int -> Int        -- <2>
two x y = x + y

one   : Int -> Int               -- <3>
one x = x

zero  : Int                      -- <4>
zero = 0
-- end::FunctionsArity[]


-- tag::FunctionsUncurried[]
two_  : (Int, Int) -> Int
two_ (x, y) = x + y
-- end::FunctionsUncurried[]


infixOrPrefix : Bool
infixOrPrefix = 1 + 2 == (+) 1 2


-- tag::FunctionsTernary[]
infix 0 ?                       -- <1>

(?) : Bool -> (Bool -> a) -> a  -- <2>
(?) bool func = bool |> func    -- <3>

(#) : a -> a -> Bool -> a
(#) yes no =
  \test ->                      -- <4>
    if test then yes else no

fakeTernary : String
fakeTernary =
  1 == 1 ? "Yes" # "No"         -- <5>
-- end::FunctionsTernary[]


-- tag::FunctionsComposition[]
-- import Text as T

ninetiesStyle1 : String -> Element
ninetiesStyle1 str =
  leftAligned (T.color blue (T.line T.Under (T.fromString str))) -- <1>

ninetiesStyle2 : String -> Element
ninetiesStyle2 =
  leftAligned << T.color blue << T.line T.Under << T.fromString -- <2>

ninetiesStyle3 : String -> Element
ninetiesStyle3 =
  T.fromString >> T.line T.Under >> T.color blue >> leftAligned -- <3>
-- end::FunctionsComposition[]


-- tag::FunctionsPipeline[]
ninetiesStylePipe : String -> Element
ninetiesStylePipe str = str
  |> T.fromString
  |> T.line T.Under
  |> T.color blue
  |> leftAligned
-- end::FunctionsPipeline[]
