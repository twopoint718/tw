module Tests exposing (..)

--import Console
import String
import Test exposing (Test, describe, test)
import Expect exposing (equal, notEqual, true)

import BasicSyntax exposing (..)
import Functions exposing (..)
import TypesAndTypeAliases as T
import Records exposing (..)


suite : Test
suite =
  describe "Crash course"
    [ describe "Basic syntax"
        [ test "Shortest word" (\() -> shortestWord ["aa", "a"] `equal` Just "a")
        , test "Import as" (\() -> true "expected empty string" usesString)
        , test "Need to round" (\() -> myNum `equal` sqrt 2)
        , test "Float division" (\() -> twopointfive `notEqual` 2)
        , test "Two ways to prepend" (\() -> true "both should be 'cat'" prepend)
        , test "Convert string to list" (\() -> true "should be list of vowels" listify)
        ]
    , describe "Types and Type Aliases"
        [ test "Maybe" (\() -> T.Just 1 `equal` T.Just 1)
        , test "Result err/ok" (\() -> T.Err "wrong" `notEqual` T.Ok "okay")
        , test "Result ok/ok" (\() -> T.Ok 1 `equal` T.Ok 1)
        , test "Damages" (\() -> T.damages `equal` [6, 6, 20])
        ]
    , describe "Functions"
        [ test "Three args" (\() -> three 1 2 3 `equal` 1)
        , test "Two args" (\() -> two 1 2 `equal` 3)
        , test "One arg (identity)" (\() -> one 1 `equal` 1)
        , test "Zero arg (constant)" (\() -> zero `equal` 0)
        , test "Uncurried args" (\() -> two' (1, 2) `equal` two 1 2)
        , test "Operators can be used infix or prefix" <|
            \() -> true "expected prefix to be the same" infixOrPrefix
        , test "Question mark" (\() -> true "should be true" (True ? identity))
        , test "Pound sign first branch" (\() -> (1 # 2) True `equal` 1)
        , test "Pound sign second branch" (\() -> (1 # 2) False `equal` 2)
        , test "Fake ternary" (\() -> fakeTernary `equal` "Yes")
        ]
    , describe "Records"
        [ test "field accessor" (\() -> .damage squid `equal` squid.damage)
        , test "literal record" (\() -> orc.damage `equal` "1d6")
        , test "mapping fields"
            <| \() -> calcHitPoints [squid, orc] `equal` [34, 10]
        , test "can't combine different types (no using squid or orc here!)"
            <| \() -> calcHitPoints [kobold] `equal` [4]
        ]
    ]
