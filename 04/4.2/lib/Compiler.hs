module Compiler where


import Control.Arrow ((>>>))

import qualified Ast as Ast
import qualified Intermediate as I
import qualified Parser as P
import qualified Typechecker as T
import qualified WebAssembly as W


pipeline :: [Ast.Def] -> String
pipeline = I.fromAst                            -- <1>
  >>> I.collapseLambdas                         -- <2>
  >>> I.eliminateLambdas []                     -- <3>
  >>> I.toWast                                  -- <4>
  >>> W.generate                                -- <5>


compileString :: String -> Either String String
compileString code = do
  defs <- P.runParse "[no file]" code           -- <6>
  _ <- T.typecheck defs                         -- <7>
  return (pipeline defs)                        -- <8>
