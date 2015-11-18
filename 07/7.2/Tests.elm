module Main where

import Console
import ElmTest exposing (..)
import String
import Task

import BasicSyntax exposing (..)
import Functions exposing (..)
import TypesAndTypeAliases as T
import Records exposing (..)
import Signals exposing (..)


tests : Test
tests =
  suite "Crash course"
    [ suite "Basic syntax"
        [ test "Shortest word" (shortestWord ["aa", "a"] `assertEqual` Just "a")
        , test "Import as" (assert usesString)
        , test "Need to round" (myNum `assertEqual` sqrt 2)
        , test "Float division" (twopointfive `assertNotEqual` 2)
        , test "Two ways to prepend" (assert prepend)
        , test "Convert string to list" (assert listify)
        ]
    , suite "Types and Type Aliases"
        [ test "Maybe" (T.Just 1 `assertEqual` T.Just 1)
        , test "Result err/ok" (T.Err "wrong" `assertNotEqual` T.Ok "okay")
        , test "Result ok/ok" (T.Ok 1 `assertEqual` T.Ok 1)
        , test "Damages" (T.damages `assertEqual` [6, 6, 20])
        ]
    , suite "Functions"
        [ test "Three args" (three 1 2 3 `assertEqual` 1)
        , test "Two args" (two 1 2 `assertEqual` 3)
        , test "One arg (identity)" (one 1 `assertEqual` 1)
        , test "Zero arg (constant)" (zero `assertEqual` 0)
        , test "Uncurried args" (two' (1, 2) `assertEqual` two 1 2)
        , test "Operators can be used infix or prefix" (assert infixOrPrefix)
        , test "Question mark" (assert (True ? identity))
        , test "Pound sign first branch" ((1 # 2) True `assertEqual` 1)
        , test "Pound sign second branch" ((1 # 2) False `assertEqual` 2)
        , test "Fake ternary" (fakeTernary `assertEqual` "Yes")
        ]
    , suite "Records"
        [ test "field accessor" (.damage squid `assertEqual` squid.damage)
        , test "literal record" (orc.damage `assertEqual` "1d6")
        , test "mapping fields"
            <| calcHitPoints [squid, orc] `assertEqual` [34, 10]
        , test "can't combine different types (no using squid or orc here!)"
            <| calcHitPoints [kobold] `assertEqual` [4]
        ]
    , suite "Signals"
        [ test "" (interruptingCow `assertEqual` interruptingCow)
        ]
    ]


port runner : Signal (Task.Task x ())
port runner =
    Console.run (consoleRunner tests)


main =
  elementRunner tests
