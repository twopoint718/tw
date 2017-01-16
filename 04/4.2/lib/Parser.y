{
{-# LANGUAGE DeriveFunctor #-}

module Parser
  ( parser
  , runParse
  , testParse
  , testParseFile
  )
where


import Lexer

import qualified Ast as Ast
import Text.Printf (printf)
}


%name parser
%tokentype { Token }
%error { happyError }
%monad { Alex }
%lexer { lexer } { Token _ TokenEOF }


%token
  COLON    { Token _ TokenColon }
  COMMA    { Token _ TokenComma }
  ELSE     { Token _ TokenElse }
  EOF      { Token _ TokenEOF }
  EQUAL    { Token _ TokenEq }
  FALSE    { Token _ TokenFalse }
  LAMBDA   { Token _ TokenLambda }
  IF       { Token _ TokenIf }
  INT      { Token _ (TokenInt $$) }
  LCURLY   { Token _ TokenLCurly }
  LESS     { Token _ TokenLess }
  LET      { Token _ TokenLet }
  LPAREN   { Token _ TokenLParen }
  MINUS    { Token _ TokenMinus }
  PLUS     { Token _ TokenPlus }
  RCURLY   { Token _ TokenRCurly }
  RPAREN   { Token _ TokenRParen }
  SEMISEMI { Token _ TokenSemiSemi }
  TARROW   { Token _ TokenTypeArrow }
  TBOOL    { Token _ TokenTypeBool }
  THEN     { Token _ TokenThen }
  TIMES    { Token _ TokenTimes }
  TINT     { Token _ TokenTypeInt }
  TRUE     { Token _ TokenTrue }
  VAR      { Token _ (TokenVar $$) }


%nonassoc ELSE
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES
%left NEG
%right TARROW


%%

-- tag::ParserFileDef[]
file :: { [Ast.Def] }
file : file_ { reverse $1 }                            -- <1>

file_ :: { [Ast.Def] }
file_
  : file_ SEMISEMI def   { $3 : $1 }                   -- <2>
  | file_ SEMISEMI       { $1 }                        -- <3>
  | def                  { [$1] }                      -- <4>
  | EOF                  { [] }                        -- <5>
  | {- empty -}          { [] }                        -- <6>

def :: { Ast.Def }
def
  : LET VAR COLON type EQUAL expr { Ast.Def $2 $4 $6 } -- <7>

type :: { Ast.Type }
type                                                   -- <8>
: TBOOL { Ast.TBool }
| TINT { Ast.TInt }
| type TARROW type { Ast.TArrow $1 $3 }
| LPAREN type RPAREN { $2 }
-- end::ParserFileDef[]

-- tag::ParserExpr[]
expr :: { Ast.Expr }
expr
  : app_expr { $1 }                                    -- <1>
  | MINUS INT { Ast.Int (-$2) }                        -- <2>
  | expr PLUS expr { Ast.Plus $1 $3 }                  -- <2>
  | expr MINUS expr { Ast.Minus $1 $3 }                -- <2>
  | expr TIMES expr { Ast.Times $1 $3 }                -- <2>
  | expr EQUAL expr { Ast.Equal $1 $3 }                -- <2>
  | expr LESS expr { Ast.Less $1 $3 }                  -- <2>
  | IF expr THEN expr ELSE expr { Ast.If $2 $4 $6 }    -- <2>
  | LAMBDA LPAREN arg_list RPAREN COLON type LCURLY expr RCURLY
      { Ast.Lambda $3 $6 $8 }                          -- <3>

arg_list :: { [(Ast.Name, Ast.Type)] }                 -- <4>
arg_list : arg_list_ { reverse $1 }

arg_list_ :: { [(Ast.Name, Ast.Type)] }                -- <4>
arg_list_
  : arg_list_ COMMA VAR COLON type { ($3, $5) : $1 }
  | VAR COLON type                 { [($1, $3)] }
  | {- empty -}                    { [] }

app_expr :: { Ast.Expr }
app_expr                                               -- <5>
  : simple_expr { $1 }
  | app_expr simple_expr { Ast.Apply $1 $2 }

simple_expr :: { Ast.Expr }
simple_expr                                            -- <6>
  : VAR { Ast.Var $1 }
  | TRUE { Ast.Bool True }
  | FALSE { Ast.Bool False }
  | INT { Ast.Int $1 }
  | LPAREN expr RPAREN { $2 }
-- end::ParserExpr[]


{
lexer :: (Token -> Alex a) -> Alex a
lexer = (>>=) alexMonadScan'


happyError :: Token -> Alex a
happyError (Token pos tok) =
  alexError' pos (printf "parse error at token '%s'" (show tok))


runParse :: FilePath -> String -> Either String [Ast.Def]
runParse = runAlex' parser


testParse :: String -> [Ast.Def]
testParse s =
  case runAlex' parser "[no file]" s of
    Left err -> error err
    Right cmds -> cmds


testParseFile :: FilePath -> IO [Ast.Def]
testParseFile path = do
  content <- readFile path
  case (runAlex' parser path content) of
    Left err -> error err
    Right cmds -> return cmds
}
