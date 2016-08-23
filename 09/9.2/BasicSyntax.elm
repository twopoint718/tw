-- tag::BasicSyntaxModuleImport[]
module BasicSyntax exposing (..)

import List exposing (all, head, sortBy)
import String exposing (length)
import String as S
import Element exposing (show)
-- end::BasicSyntaxModuleImport[]


-- tag::BasicSyntaxOverview[]
shortestWord : List String -> Maybe String
shortestWord strings = head (sortBy length strings)
-- end::BasicSyntaxOverview[]


-- tag::BasicSyntaxQualified[]
usesString : Bool
usesString = S.isEmpty ""
-- end::BasicSyntaxQualified[]


-- tag::BasicSyntaxConvert[]
myNum : Float
myNum = sqrt (toFloat (5 // 2))
-- end::BasicSyntaxConvert[]


-- tag::BasicSyntaxFloatDiv[]
twopointfive : Float
twopointfive = 5 / 2
-- end::BasicSyntaxFloatDiv[]


-- tag::BasicSyntaxPrepend[]
prepend : Bool
prepend =
  S.cons 'c' "at" == S.fromChar 'c' ++ "at"
-- end::BasicSyntaxPrepend[]


-- tag::BasicSyntaxToList[]
listify : Bool
listify =
  S.toList "aeiou" == ['a', 'e', 'i', 'o', 'u']
-- end::BasicSyntaxToList[]
