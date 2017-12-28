{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies    #-}

module Intermediate where


import           Control.Arrow         ((>>>))
import           Control.Monad.Gen
import           Control.Monad.Writer  (WriterT, runWriterT, tell)
import           Data.Functor.Foldable (Base, Corecursive, Recursive, cata,
                                        embed, project)
import           Data.List             (foldl')
import           Data.Set              (Set)
import           Prelude               hiding (exp)

import qualified Ast
import qualified Data.Set              as Set
import qualified WebAssembly           as W


type Name = String


data Def = Def Name [Name] IR
  deriving (Eq, Show)


-- tag::IntermediateIR[]
data IR
  = Apply IR IR
  | Bool Bool
  | Equal IR IR
  | If IR IR IR
  | Int Int
  | Lambda [Name] IR
  | Less IR IR
  | Minus IR IR
  | Plus IR IR
  | Times IR IR
  | Var Name
  deriving (Show, Eq)
-- end::IntermediateIR[]


data IRF a
  = ApplyF a a
  | BoolF Bool
  | EqualF a a
  | IfF a a a
  | IntF Int
  | LambdaF [Name] a
  | LessF a a
  | MinusF a a
  | PlusF a a
  | TimesF a a
  | VarF Name
  deriving (Functor, Show, Eq)


type instance Base IR = IRF


instance Recursive IR where
  project expr =
    case expr of
      Apply e1 e2    -> ApplyF e1 e2
      Bool b         -> BoolF b
      Equal e1 e2    -> EqualF e1 e2
      If e1 e2 e3    -> IfF e1 e2 e3
      Int i          -> IntF i
      Lambda names e -> LambdaF names e
      Less e1 e2     -> LessF e1 e2
      Minus e1 e2    -> MinusF e1 e2
      Plus e1 e2     -> PlusF e1 e2
      Times e1 e2    -> TimesF e1 e2
      Var name       -> VarF name


instance Corecursive IR where
  embed expr =
    case expr of
      ApplyF e1 e2    -> Apply e1 e2
      BoolF b         -> Bool b
      EqualF e1 e2    -> Equal e1 e2
      IfF e1 e2 e3    -> If e1 e2 e3
      IntF i          -> Int i
      LambdaF names e -> Lambda names e
      LessF e1 e2     -> Less e1 e2
      MinusF e1 e2    -> Minus e1 e2
      PlusF e1 e2     -> Plus e1 e2
      TimesF e1 e2    -> Times e1 e2
      VarF name       -> Var name


-- tag::IntermediateClosureConvert[]
freeVars :: IR -> Set Name
freeVars = cata alg where                                        -- <1>
  alg (ApplyF f vs)   = f `Set.union` vs                         -- <2>
  alg (BoolF _)       = Set.empty
  alg (EqualF e1 e2)  = e1 `Set.union` e2
  alg (IfF x y z)     = x `Set.union` y `Set.union` z
  alg (IntF _)        = Set.empty
  alg (LambdaF vs e1) = e1 `Set.difference` Set.fromList vs      -- <3>
  alg (LessF x y)     = x `Set.union` y
  alg (MinusF x y)    = x `Set.union` y
  alg (PlusF x y)     = x `Set.union` y
  alg (TimesF x y)    = x `Set.union` y
  alg (VarF v)        = Set.singleton v                          -- <4>

applyTo :: IR -> [Name] -> IR                                    -- <5>
applyTo = foldl' (\expr name -> Apply expr (Var name))

closureConvert :: [Name] -> IR -> IR
closureConvert globals = cata alg                                -- <6>
  where
    alg (LambdaF vs e) =                                         -- <7>
      let boundVars = Set.fromList (globals ++ vs)
          vars = freeVars e `Set.difference` boundVars
          vars' = Set.toList vars
      in Lambda (vars' ++ vs) e `applyTo` vars'
    alg e = embed e
-- end::IntermediateClosureConvert[]


-- tag::IntermediateLiftLambda[]
type ClosM = WriterT [Def] (Gen Integer)

liftLambda :: IR -> ClosM IR
liftLambda = cata alg where                                      -- <1>
  alg (ApplyF l r)      = Apply <$> l <*> r
  alg (BoolF b)         = return (Bool b)
  alg (EqualF e1 e2)    = Equal <$> e1 <*> e2
  alg (IfF e1 e2 e3)    = If <$> e1 <*> e2 <*> e3
  alg (IntF i)          = return (Int i)
  alg (LambdaF names e) = do                                     -- <2>
    newSym <- gensym                                             -- <3>
    newDef <- Def newSym names <$> e                             -- <4>
    tell [newDef]                                                -- <5>
    return (Var newSym)                                          -- <6>
  alg (LessF e1 e2)     = Less <$> e1 <*> e2
  alg (MinusF e1 e2)    = Minus <$> e1 <*> e2
  alg (PlusF e1 e2)     = Plus <$> e1 <*> e2
  alg (TimesF e1 e2)    = Times <$> e1 <*> e2
  alg (VarF v)          = return (Var v)
-- end::IntermediateLiftLambda[]


gensym :: ClosM Name
gensym =
  gen >>= \i -> return ("gensym" ++ show i)


-- tag::IntermediateLambdaWrangling[]
collapseLambdas :: [Def] -> [Def]
collapseLambdas = map collapse where
  collapse def =
    case def of
      Def name params (Lambda params' e) ->                      -- <1>
        Def name (params ++ params') e
      def' -> def'

smash :: IR -> IR
smash = cata alg where
  alg (LambdaF names (Lambda names' e)) =                        -- <2>
    Lambda (names ++ names') e
  alg e = embed e

eliminateLambdas :: [Name] -> [Def] -> [Def]
eliminateLambdas globals = concatMap helper                      -- <3>
  where
    run = runGen . runWriterT                                    -- <4>
    xform = closureConvert globals >>> smash >>> liftLambda      -- <5>

    helper (Def name params expr) =                              -- <6>
      let (newExpr, newDefs) = (run . xform) expr
      in Def name params newExpr : newDefs
-- end::IntermediateLambdaWrangling[]


--------------------------------------------------------------------------------
-- In/out conversions


fromAst :: [Ast.Def] -> [Def]
fromAst [] = []
fromAst (Ast.Def name _ e : defs) =
  Def name [] (fromAstExpr e) : fromAst defs


fromAstExpr :: Ast.Expr -> IR
fromAstExpr expr =
  case expr of
    Ast.Apply e1 e2 -> Apply (fromAstExpr e1) (fromAstExpr e2)
    Ast.Bool bool -> Bool bool
    Ast.Equal e1 e2 -> Equal (fromAstExpr e1) (fromAstExpr e2)
    Ast.Lambda names _ expr' -> Lambda (map fst names) (fromAstExpr expr')
    Ast.If e1 e2 e3 -> If (fromAstExpr e1) (fromAstExpr e2) (fromAstExpr e3)
    Ast.Int i -> Int i
    Ast.Less e1 e2 -> Less (fromAstExpr e1) (fromAstExpr e2)
    Ast.Minus e1 e2 -> Minus (fromAstExpr e1) (fromAstExpr e2)
    Ast.Plus e1 e2 -> Plus (fromAstExpr e1) (fromAstExpr e2)
    Ast.Times e1 e2 -> Times (fromAstExpr e1) (fromAstExpr e2)
    Ast.Var name -> Var name


-- tag::IntermediateToWAST[]
toWast :: [Def] -> W.Module
toWast ds = W.Module funcs Nothing where                         -- <1>
  funcs :: [W.Func]
  funcs = map toFunc ds                                          -- <2>

  toFunc :: Def -> W.Func
  toFunc (Def name names ir) = W.Func                            -- <3>
    (W.mkName name)
    (toParams names)
    (W.Result W.I64)
    (fromIR ir)

  toParams :: [Name] -> [W.Param]                                -- <4>
  toParams = map (\name -> (W.mkName name, W.I64))

  errNonNamed = error "code gen: calling a non-named function"   -- <5>
  errBadLambda = error "code gen: lambdas should be eliminated"

  true = W.Const 1
  false = W.Const 0

  fromIR :: IR -> W.Expr
  fromIR = cata alg where
    alg :: IRF W.Expr -> W.Expr
    alg (ApplyF (W.GetLocal l) r) = W.Call l [r]                 -- <6>
    alg (ApplyF _ _)              = errNonNamed
    alg (BoolF b)                 = if b then true else false
    alg (EqualF e1 e2)            = W.Equal e1 e2
    alg (IfF e1 e2 e3)            = W.If e1 e2 e3
    alg (IntF i)                  = W.Const (fromIntegral i)     -- <7>
    alg (LambdaF _ _)             = errBadLambda
    alg (LessF e1 e2)             = W.Lt e1 e2
    alg (MinusF e1 e2)            = W.Sub e1 e2
    alg (PlusF e1 e2)             = W.Add e1 e2
    alg (TimesF e1 e2)            = W.Mul e1 e2
    alg (VarF v)                  = W.GetLocal (W.mkName v)      -- <8>
-- tag::IntermediateToWAST[]
