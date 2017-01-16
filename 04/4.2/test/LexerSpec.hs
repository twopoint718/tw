{-# LANGUAGE QuasiQuotes #-}

module LexerSpec where

import Test.Hspec
import Lexer
import Text.Heredoc


main :: IO ()
main = hspec spec


expr0 :: String
expr0 = ""


expected0 :: [TokenName]
expected0 = []


expr1 :: String
expr1 = "3 + (if 5 < 6 then 10 else 100) ;;"


expected1 :: [TokenName]
expected1 =
  [TokenInt 3, TokenPlus, TokenLParen, TokenIf, TokenInt 5, TokenLess,
   TokenInt 6, TokenThen, TokenInt 10, TokenElse, TokenInt 100,
   TokenRParen, TokenSemiSemi]


expr2 :: String
expr2 = "fun id (x : int) : int { x }"


expected2 :: [TokenName]
expected2 =
  [TokenLambda, TokenVar "id", TokenLParen, TokenVar "x", TokenColon,
   TokenTypeInt, TokenRParen, TokenColon, TokenTypeInt, TokenLCurly,
   TokenVar "x", TokenRCurly]


expr3 :: String
expr3 = "let x = 14 ;;"


expected3 :: [TokenName]
expected3 =
  [TokenLet, TokenVar "x", TokenEq, TokenInt 14, TokenSemiSemi]


expr4 :: String
expr4 = [here|
  3 + (if 5 < 6 then 10 else 100) ;;

  let x = 14 ;;

  let fact =
    fun f (n : int) : int {
      if n = 0 then 1 else n * f (n-1) ;;
    }

  fact 10 ;;
|]


expected4 :: [TokenName]
expected4 = expected1 ++ expected3 ++
  [TokenLet, TokenVar "fact", TokenEq, TokenLambda, TokenVar "f",
   TokenLParen, TokenVar "n", TokenColon, TokenTypeInt, TokenRParen,
   TokenColon, TokenTypeInt, TokenLCurly, TokenIf, TokenVar "n",
   TokenEq, TokenInt 0, TokenThen, TokenInt 1, TokenElse, TokenVar "n",
   TokenTimes, TokenVar "f", TokenLParen, TokenVar "n", TokenMinus,
   TokenInt 1, TokenRParen, TokenSemiSemi, TokenRCurly, TokenVar "fact",
   TokenInt 10, TokenSemiSemi]


spec :: Spec
spec = do
  describe "Lexing" $ do
    it expr0 $
      testLexer expr0 `shouldBe` expected0

    it expr1 $
      testLexer expr1 `shouldBe` expected1

    it expr2 $
      testLexer expr2 `shouldBe` expected2

    it expr3 $
      testLexer expr3 `shouldBe` expected3

    it "full example" $
      testLexer expr4 `shouldBe` expected4


--------------------------------------------------------------------------------
-- Util


testLexer :: String -> [TokenName]
testLexer s =
  case runAlex s alexScanAll of
    Left err -> error err
    Right tokens -> fmap unToken tokens

  where
    unToken (Token _ tokenName) = tokenName
