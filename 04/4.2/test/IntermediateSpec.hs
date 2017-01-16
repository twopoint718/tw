{-# LANGUAGE OverloadedLists #-}

module IntermediateSpec where


import Test.Hspec
--import qualified Debug.Trace as D

import Intermediate


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  context "closureConvert" $
    it "\foo -> foo (\bar -> bar foo)" $ do
      closureConvert [] $
        Lambda ["foo"]
          (Apply (Var "foo")
                 (Lambda ["bar"]
                   (Apply (Var "bar")
                          (Var "foo"))))
      `shouldBe`
      Lambda ["foo"]
        (Apply (Var "foo")
               (Apply (Lambda ["foo", "bar"]
                        (Apply (Var "bar")
                               (Var "foo")))
                      (Var "foo")))


  context "eliminateLambdas" $
    it "..." $
      eliminateLambdas ["b"]
        [Def "foo" ["x"] $
          Lambda [] $
            (Plus (Var "x") (Var "b"))]
      `shouldBe`
      [ Def "foo" ["x"] (Apply (Var "gensym1") (Var "x"))
      , Def "gensym1" ["x"] (Plus (Var "x") (Var "b"))
      ]
