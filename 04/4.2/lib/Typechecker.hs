{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Typechecker where


import Data.List (lookup)
import Data.These
import Control.Monad.Chronicle

import Ast
import Prelude hiding (True, False)
import Text.Printf (printf)


type Context = [(String, Type)]


type CheckResult = Chronicle [String] (Maybe Type)


withType :: Maybe Type -> (Type -> CheckResult) -> CheckResult
withType = flip (maybe (typeError "could not determine expression's type"))


typeError :: String -> CheckResult
typeError msg = disclose [msg]


keepErrors :: [String] -> CheckResult
keepErrors = disclose


typeOk :: Type -> CheckResult
typeOk = return . Just


-- tag::TypecheckerTypecheck[]
typecheck :: [Def] -> Either String ()
typecheck defs =
  case runChronicle (typeOfDefs [] defs) of       -- <1>
    This errs -> Left (unwords errs)              -- <2>
    That _ -> Right ()                            -- <3>
    These errs _ -> Left (unwords errs)           -- <4>

typeOfDefs :: Context -> [Def] -> CheckResult
typeOfDefs _ [] = return Nothing
typeOfDefs ctx ((Def name defType expr):defs) = do
  let ctx' = (name, defType) : ctx
  _ <- check ctx' defType expr                    -- <5>
  typeOfDefs ctx' defs                            -- <6>
-- end::TypecheckerTypecheck[]


-- tag::TypecheckerCheckExpression[]
check :: Context -> Type -> Expr -> CheckResult
check ctx ty expr =
  case runChronicle (typeOf ctx expr) of                      -- <1>
    This errs -> keepErrors errs                              -- <2>
    That mty' -> withType mty' $ \ty' ->                      -- <3>
      if ty /= ty'
      then typeError $                                        -- <4>
        printf "Expecting '%s' to have type '%s', but found '%s'"
          (show expr) (show ty) (show ty')
      else
        typeOk ty'                                            -- <5>
    These errs _ -> keepErrors errs                           -- <6>

typeOf :: Context -> Expr -> CheckResult
typeOf ctx expr =
  case expr of                                                -- <7>
    Apply fun arg -> do
      mfunType <- typeOf ctx fun
      margType <- typeOf ctx arg
      case (mfunType, margType) of
        (Just (TArrow inputType outputType), Just argType) -> -- <8>
          if inputType == argType
          then typeOk outputType
          else typeError $
            printf "(%s -> %s) is applied to %s. That won't work."
              (show inputType) (show outputType) (show argType)
        (Just ty1, Just _) ->
          typeError $ printf "%s is nat a function type" (show ty1)
        (Just ty, Nothing) -> do
          _ <- typeError $
            printf "%s is being used as a function. It isn't" (show ty)
          typeError "cannot determine type of the argument"
        (Nothing, Just _) ->
          typeError "cannot determine type of the function"
        (Nothing, Nothing) ->
          typeError
            "cannot determine type of the function or the argument"

    Bool _ -> typeOk TBool                                    -- <9>

    Equal e1 e2 -> do                                         -- <10>
      _ <- check ctx TInt e1
      _ <- check ctx TInt e2
      typeOk TBool

    Lambda names retType e -> do                              -- <11>
      let
        nestedSigType = foldr TArrow retType (map snd names)
        ctx' = ("__self__", nestedSigType) : (names ++ ctx)
      _ <- check ctx' retType e
      typeOk nestedSigType

    If cond e1 e2 -> do                                       -- <12>
      _ <- check ctx TBool cond
      me1Type <- typeOf ctx e1
      case me1Type of
        Nothing -> typeError "couldn't determine THEN branch's type"
        Just e1Type -> do
          _ <- check ctx e1Type e2
          typeOk e1Type

    Int _ -> typeOk TInt

    Less e1 e2 -> do
      _ <- check ctx TInt e1
      _ <- check ctx TInt e2
      typeOk TBool

    Minus e1 e2 -> do
      _ <- check ctx TInt e1
      _ <- check ctx TInt e2
      typeOk TInt

    Plus e1 e2 -> do
      _ <- check ctx TInt e1
      _ <- check ctx TInt e2
      typeOk TInt

    Times e1 e2 -> do
      _ <- check ctx TInt e1
      _ <- check ctx TInt e2
      typeOk TInt

    Var s ->                                                  -- <13>
      case lookup s ctx of
        Nothing -> typeError $
          printf "Unknown variable %s" s

        Just s' ->
          typeOk s'
-- end::TypecheckerCheckExpression[]
