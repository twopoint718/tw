{-# LANGUAGE QuasiQuotes #-}

module CompilerSpec where


import           Compiler
import           Test.Hspec                     (Spec, context, hspec, it)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           Text.Heredoc


main :: IO ()
main = hspec spec


spec :: Spec
spec =
  context "compileString" $ do
    it "simple" $
      compileString "[no file]" "let main : int = 1"
      `shouldBe` normalize
      [str|(module
          |  (export "$main" (func $main))
          |
          |  (func $main (result i64)
          |    (i64.const 1)))
          |]

    it "fact" $
      compileString "[no file]"
        [str|let fact : int -> int =
            |  fun(n : int) : int {
            |    if n = 0 then 1 else n * fact (n-1)
            |  };;
            |
            |let main : int = fact 10;;
            |]
      `shouldBe` normalize
      [str|(module
          |  (export "$fact" (func $fact))
          |  (export "$main" (func $main))
          |
          |  (func $fact (param $n i64) (result i64)
          |    (if i64 (i64.eq (get_local $n) (i64.const 0))
          |      (i64.const 1)
          |      (i64.mul
          |        (get_local $n)
          |        (call $fact (i64.sub (get_local $n) (i64.const 1))))))
          |
          |  (func $main (result i64)
          |    (call $fact (i64.const 10))))
          |]


--------------------------------------------------------------------------------
-- util


normalize :: String -> Either String String
normalize = Right . unwords . words
