module Main exposing (..)

import Test exposing (Test, describe, test)
import Expect

import BasicSyntax exposing (..)
import Functions exposing (..)
import TypesAndTypeAliases as T
import Records exposing (..)


suite : Test
suite =
  describe "Crash course"
    [ describe "Basic syntax"
        [ test "Shortest word" <| \_ ->
            shortestWord ["aa", "a"] |> Expect.equal (Just "a")
        , test "Import as" <| \_ ->
            usesString |> Expect.true "expected empty string"
        , test "Need to round" <| \_ ->
            myNum |> Expect.equal (sqrt 2)
        , test "Float division" <| \_ ->
            twopointfive |> Expect.notEqual 2
        , test "Two ways to prepend" <| \_ ->
            prepend |> Expect.true "both should be 'cat'"
        , test "Convert string to list" <| \_ ->
            listify |> Expect.true "should be list of vowels"
        ]
    , describe "Types and Type Aliases"
        [ test "Maybe" <| \_ ->
            T.Just 1 |> Expect.equal (T.Just 1)
        , test "Result err/ok" <| \_ ->
            T.Err "wrong" |> Expect.notEqual (T.Ok "ok")
        , test "Result ok/ok" <| \_ ->
            T.Ok 1 |> Expect.equal (T.Ok 1)
        , test "Damages" <| \_ ->
            T.damages |> Expect.equal [6, 6, 20]
        ]
    , describe "Functions"
        [ test "Three args" <| \_ ->
            three 1 2 3 |> Expect.equal 1
        , test "Two args" <| \_ ->
            two 1 2 |> Expect.equal 3
        , test "One arg (identity)" <| \_ ->
            one 1 |> Expect.equal 1
        , test "Zero arg (constant)" <| \_ ->
            zero |> Expect.equal 0
        , test "Uncurried args" <| \_ ->
            two_ (1, 2) |> Expect.equal 3
        , test "Operators can be used infix or prefix" <|
            \_ ->
            Expect.true "expected prefix to be the same" infixOrPrefix
        , test "Question mark" <| \_ ->
            Expect.true "should be true" (True ? identity)
        , test "Pound sign first branch" <| \_ ->
            ((1 # 2) True) |> Expect.equal 1
        , test "Pound sign second branch" <| \_ ->
            ((1 # 2) False) |> Expect.equal 2
        , test "Fake ternary" <| \_ ->
            fakeTernary |> Expect.equal "Yes"
        ]
    , describe "Records"
        [ test "field accessor" <| \_ ->
            .damage squid |> Expect.equal "3d6"
        , test "literal record" <| \_ ->
            orc.damage |> Expect.equal "1d6"
        , test "mapping fields" <| \_ ->
            Expect.equal (calcHitPoints [squid, orc]) [34, 10]
        , test "can't combine different types (no using squid or orc here!)" <| \_ ->
            Expect.equal (calcHitPoints [kobold]) [4]
        ]
    ]
