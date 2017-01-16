module ParserSpec where


import Test.Hspec
import Parser (testParse, testParseFile)
import Ast
import Control.Monad.IO.Class (liftIO)


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "basic ops" $ do
    it "let x : int = 1" $
      testParse "let x : int = 1" `shouldBe` (e $ Int 1)

    it "let x : int = (1 + 2)" $
      testParse "let x : int = (1 + 2)" `shouldBe` (e $ Plus (Int 1) (Int 2))

    it "let x : int = 1" $
      testParse "let x : int = 1" `shouldBe` [Def "x" TInt (Int 1)]

    it "let x : int = 1 - 2" $
      testParse "let x : int = 1 - 2" `shouldBe` (e $ Minus (Int 1) (Int 2))

    it "let x : int = 1 + 2" $
      testParse "let x : int = 1 + 2" `shouldBe` (e $ Plus (Int 1) (Int 2))

    it "let x : int = 1 * 2" $
      testParse "let x : int = 1 * 2" `shouldBe` (e $ Times (Int 1) (Int 2))

    it "let x : int = x" $
      testParse "let x : int = x" `shouldBe` (e $ Var "x")


  describe "associativity" $
    it "let x : int = 1 + 2 * 3" $
      testParse "let x : int = 1 + 2 * 3" `shouldBe` (e $ Plus (Int 1) (Times (Int 2) (Int 3)))


  describe "file processing" $ do
    it "expr1.gos" $ do
      cmds <- liftIO (testParseFile "./test/fixtures/expr1.gos")
      cmds `shouldBe` (e $ Plus (Int 1) (Int 2))

    it "expr2.gos" $ do
      cmds <- liftIO (testParseFile "./test/fixtures/expr2.gos")
      cmds `shouldBe` expr2Expected


  describe "variable numbers of args" $ do
    it "let x:int = fun():bool { false }" $
      testParse "let x:int = fun():bool { false }"
      `shouldBe`
      [ Def "x" TInt (Lambda [] TBool (Bool False)) ]

    it "let x : int = fun(x:int, y:int):int { x + y * 2 }" $
      testParse "let x : int = fun(x:int, y:int):int { x + y * 2 }"
      `shouldBe`
      [ Def "x" TInt
          (Lambda [("x", TInt), ("y", TInt)] TInt
            (Plus (Var "x") (Times (Var "y") (Int 2))))
      ]


------------------------------------------------------------------------
-- Helpers


expr2Expected :: [Def]
expr2Expected =
  [ Def "z" TInt (Plus (Int 3) (If (Less (Int 5) (Int 6)) (Int 10) (Int 100)))
  , Def "x" TInt (Int 14)
  , Def "fact" (TArrow TInt TInt) (Lambda [("n", TInt)] TInt
                 (If (Equal (Var "n") (Int 0))
                   (Int 1)
                   (Times (Var "n") (Apply (Var "fact") (Minus (Var "n") (Int 1))))))
  , Def "main" TInt (Apply (Var "fact") (Int 10))
  ]


e :: Expr -> [Def]
e expr = [Def "x" TInt expr]

withFixture :: FilePath -> (String -> IO ()) -> IO ()
withFixture path callback = do
  str <- readFile ("./test/fixtures/" ++ path)
  callback str
  return ()
