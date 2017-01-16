module TypecheckerSpec where


import Prelude hiding (True, False)
import qualified Prelude as P
import Test.Hspec
import Data.These
import Control.Monad.Chronicle

import Typechecker
import Ast


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "type checking expressions" $ do
    it "if 1 = 1 then false else true;;" $
      testExpr ifexpr `shouldBe` That TBool

    it "(fun (x:int) : int { x + x }) 5;;" $
      testExpr app `shouldBe` That TInt

    it "(fun (x:int) : int { x + x });;" $
      testExpr fun `shouldBe` That (TArrow TInt TInt)

    it "(fun (x:int) : bool { x + x });;" $
      testExpr badfun1 `shouldBe` This
        ["Expecting 'x + x' to have type 'bool', but found 'int'"]

    it "(fun double (x:bool) : bool { x + x });;" $
      testExpr badfun2 `shouldBe` This
        [ "Expecting 'x' to have type 'int', but found 'bool'"
        , "Expecting 'x' to have type 'int', but found 'bool'"
        ]


  describe "type checking commands" $ do
    it "let fact : int -> int = fun(n:int):int { if n = 0 then 1 else n * fact (n-1) }" $
      testCommand definitionAndCall `shouldBe` That Nothing

    it "let fact:int -> bool = fun(n:int):int { if n = 0 then 1 else n * fact (n-1) };; let main:int = fact 10" $
      testCommand definitionAndCallBad `shouldBe` This
        [ "Expecting 'fact (n - 1)' to have type 'int', but found 'bool'"
        , "Expecting 'fact (10)' to have type 'int', but found 'bool'"
        ]


--------------------------------------------------------------------------------
-- Test expressions


ifexpr, fun, badfun1, badfun2, app :: Expr
ifexpr = If (Equal (Int 1) (Int 1)) (Bool P.False) (Bool P.True)
fun = Lambda [("x", TInt)] TInt (Plus (Var "x") (Var "x"))
badfun1 = Lambda [("x", TInt)] TBool (Plus (Var "x") (Var "x"))
badfun2 = Lambda [("x", TBool)] TBool (Plus (Var "x") (Var "x"))
app = Apply fun (Int 5)


definitionAndCall :: [Def]
definitionAndCall =
  [ Def "fact" (TArrow TInt TInt) (Lambda [("n", TInt)] TInt
                (If (Equal (Var "n") (Int 0))
                    (Int 1)
                    (Times (Var "n") (Apply (Var "fact") (Minus (Var "n") (Int 1))))))
  , Def "main" TInt (Apply (Var "fact") (Int 10))
  ]


definitionAndCallBad :: [Def]
definitionAndCallBad =
  [ Def "fact" (TArrow TInt TBool) (Lambda [("n", TInt)] TInt
                (If (Equal (Var "n") (Int 0))
                    (Int 1)
                    (Times (Var "n") (Apply (Var "fact") (Minus (Var "n") (Int 1))))))
  , Def "main" TInt (Apply (Var "fact") (Int 10))
  ]


testExpr :: Expr -> These [String] Type
testExpr expr =
  case runChronicle (typeOf [] expr) of
    This errs -> This errs
    That mty -> maybe (This ["couldn't get type"]) That mty
    These errs _ -> This errs


testCommand :: [Def] -> These [String] (Maybe Type)
testCommand defs =
  case runChronicle (typeOfDefs [] defs) of
    This errs -> This errs
    That mty -> That mty
    These errs _ -> This errs
