{-# LANGUAGE QuasiQuotes #-}

module WebAssemblySpec where

import           Test.Hspec
import           Text.Heredoc

import           WebAssembly


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "generateFunc" $
    it "func" $
      generateFunc
        (Func (Name "dec") [(Name "i", I64)] (Result I64)
          (If (Lt (GetLocal (Name "i")) (Const 0))
            (Const 0)
            (Sub (GetLocal (Name "i")) (Const 1))))
      `shouldBe`
      normalize
        [str|(func $dec (param $i i64) (result i64)
            |  (if i64 (i64.lt (get_local $i) (i64.const 0))
            |    (i64.const 0)
            |    (i64.sub (get_local $i) (i64.const 1))))
            |]


  describe "generate" $
    it "should work" $
      generate (Module
        [(Func (Name "dec") [(Name "i", I64)] (Result I64)
          (If (Lt (GetLocal (Name "i")) (Const 0))
            (Const 0)
            (Sub (GetLocal (Name "i")) (Const 1))))
        ] Nothing)
      `shouldBe`
      normalize
        [str|(module
            |  (export "$dec" (func $dec))
            |
            |  (func $dec (param $i i64) (result i64)
            |    (if i64 (i64.lt (get_local $i) (i64.const 0))
            |      (i64.const 0)
            |      (i64.sub (get_local $i) (i64.const 1)))))
            |]


--------------------------------------------------------------------------------
-- Util


normalize :: String -> String
normalize = trim . concat . lines


trim :: String -> String
trim = unwords . words
