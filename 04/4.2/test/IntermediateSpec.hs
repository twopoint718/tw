{-# LANGUAGE OverloadedLists #-}

module IntermediateSpec where


import           Intermediate
import           Test.Hspec


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  context "closureConvert" $
    it "\foo -> foo (\bar -> bar foo)" $
      closureConvert [] inputExpr
      `shouldBe`
      expectedExpr

  context "eliminateLambdas" $
    it "..." $
      eliminateLambdas ["b"]
        [Def "foo" ["x"] $
          Lambda [] $
            Plus (Var "x") (Var "b")]
      `shouldBe`
      [ Def "foo" ["x"] (Apply (Var "gensym1") (Var "x"))
      , Def "gensym1" ["x"] (Plus (Var "x") (Var "b"))
      ]

inputExpr :: IR
inputExpr =
  Lambda ["foo"]
    (Apply (Var "foo")
      (Lambda ["bar"]
        (Apply (Var "bar") (Var "foo"))))

expectedExpr :: IR
expectedExpr =
  Lambda ["foo"]
    (Apply (Var "foo")
      (Apply
        (Lambda ["foo", "bar"]
          (Apply (Var "bar") (Var "foo")))
        (Var "foo")))
