{-# LANGUAGE QuasiQuotes #-}

module ToElmSpec where


import           Test.Hspec
import           Text.Heredoc (str)

import           Ride         (Ride)
import           ToElm        (toElm)


spec :: Spec
spec =
    describe "Generated type alias" $
        it "is valid Elm code" $
            toElm (undefined :: Ride) `shouldBe` expected


expected :: String
expected =
    [str|type alias Ride =
        |    { id : Int
        |    , user : String
        |    , date : Date
        |    , distance : Float
        |    }
        |]
