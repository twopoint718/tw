{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module WebAssembly where


import Data.Int (Int64)
import Text.Printf (printf)
import Data.Functor.Foldable
  ( Base
  , Corecursive, embed
  , Recursive, project
  , cata
  )

import qualified Text.Printf as P


data Module = Module [Func] (Maybe [Test])


data Func = Func Name [Param] Result Expr


data Test = Test Name Int64


data Type = I64


data Expr
  = Add Expr Expr
  | Call Name [Expr]
  | Const Int64
  | Equal Expr Expr
  | GetGlobal Name
  | GetLocal Name
  | If Expr Expr Expr
  | Lt Expr Expr
  | Mul Expr Expr
  | Return Expr
  | Sub Expr Expr
  | Var Name


data ExprF a
  = AddF a a
  | CallF Name [a]
  | ConstF Int64
  | EqualF a a
  | GetGlobalF Name
  | GetLocalF Name
  | IfF a a a
  | LtF a a
  | MulF a a
  | ReturnF a
  | SubF a a
  | VarF Name
  deriving (Functor)


type instance Base Expr = ExprF


instance Recursive Expr where
  project expr =
    case expr of
      Add e1 e2      -> AddF e1 e2
      Call name es   -> CallF name es
      Const word64   -> ConstF word64
      GetLocal name  -> GetLocalF name
      GetGlobal name -> GetGlobalF name
      If e1 e2 e3    -> IfF e1 e2 e3
      Lt e1 e2       -> LtF e1 e2
      Return e       -> ReturnF e
      Sub e1 e2      -> SubF e1 e2
      Equal e1 e2    -> EqualF e1 e2
      Var name       -> VarF name
      Mul e1 e2      -> MulF e1 e2


instance Corecursive Expr where
  embed expr =
    case expr of
      AddF e1 e2      -> Add e1 e2
      CallF name es   -> Call name es
      ConstF word64   -> Const word64
      GetLocalF name  -> GetLocal name
      GetGlobalF name -> GetGlobal name
      IfF e1 e2 e3    -> If e1 e2 e3
      LtF e1 e2       -> Lt e1 e2
      ReturnF e       -> Return e
      SubF e1 e2      -> Sub e1 e2
      EqualF e1 e2    -> Equal e1 e2
      VarF name       -> Var name
      MulF e1 e2      -> Mul e1 e2


newtype Name = Name String


mkName :: String -> Name
mkName = Name


instance P.PrintfArg Name where
  formatArg (Name name) fmt
    | P.fmtChar (P.vFmt 'n' fmt) == 'n'
        = P.formatString ("$" ++ name) (fmt { P.fmtChar = 's' })
  formatArg _ fmt = P.errorBadFormat $ P.fmtChar fmt


newtype Result = Result Type


type Param = (Name, Type)


generate :: Module -> String
generate (Module funcs tests) =
  let
    exports = unwords (map generateExport funcs)
    funcs' = unwords (map generateFunc funcs)
  in
    case tests of
      Nothing -> printf "(module %s %s)"
        exports
        funcs'

      Just tests' -> printf "(module %s %s) %s"
        exports
        funcs'
        (unwords (map generateTest tests'))


generateExport :: Func -> String
generateExport (Func name _ _ _) =
  printf "(export \"%n\" (func %n))" name name


generateFunc :: Func -> String
generateFunc (Func name [] result expr) =
  printf "(func %n %s %s)"
    name
    (generateResult result)
    (generateExpr expr)
generateFunc (Func name params result expr) =
  printf "(func %n %s %s %s)"
    name
    (generateParams params)
    (generateResult result)
    (generateExpr expr)


generateParams :: [Param] -> String
generateParams = unwords . map generateParam
  where
    generateParam (name, ty) =
      printf "(param %n %s)" name (generateType ty)


generateResult :: Result -> String
generateResult (Result ty)= printf "(result %s)" (generateType ty)


-- tag::WebAssemblyGenerateExpr[]
generateExpr :: Expr -> String
generateExpr expr = cata alg expr where
  alg (AddF e1 e2)       = printf "(i64.add %s %s)" e1 e2
  alg (CallF name exprs) = printf "(call %n %s)" name (unwords exprs)
  alg (ConstF i)         = printf "(i64.const %d)" i
  alg (EqualF e1 e2)     = printf "(i64.eq %s %s)" e1 e2
  alg (GetGlobalF name)  = printf "(get_global %n)" name
  alg (GetLocalF name)   = printf "(get_local %n)" name
  alg (IfF e1 e2 e3)     = printf "(if i64 %s %s %s)" e1 e2 e3
  alg (LtF e1 e2)        = printf "(i64.lt %s %s)" e1 e2
  alg (MulF e1 e2)       = printf "(i64.mul %s %s)" e1 e2
  alg (ReturnF e)        = printf "(return %s)" e
  alg (SubF e1 e2)       = printf "(i64.sub %s %s)" e1 e2
  alg (VarF name)        = printf "%n" name
-- end::WebAssemblyGenerateExpr[]



generateTest :: Test -> String
generateTest (Test name result) =
  printf "(assert_return (invoke \"%n\") (i64.const %s))"
    name
    (show result)


generateType :: Type -> String
generateType I64 = "i64"
