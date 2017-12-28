{-# LANGUAGE QuasiQuotes #-}

module ToDecoderSpec where


import           Test.Hspec
import           Text.Heredoc (str)

import           Ride         (Ride)
import           ToDecoder


spec :: Spec
spec =
    describe "Generated decoder" $
        it "is valid Elm code" $
            toDecoder (undefined :: Ride) `shouldBe` expected


expected :: String
expected =
    [str|ride : Decoder Ride
        |ride = succeed Ride
        |    <*> field "id" int
        |    <*> field "user" string
        |    <*> field "date" date
        |    <*> field "distance" float
        |]
