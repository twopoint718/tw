{
module Lexer where


import Control.Monad (liftM)
import Text.Printf (printf)
}


%wrapper "monadUserState"

-- tag::LexerTokens[]
$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  "--".*                        ;
  $white+                       ;
  ":"                           { mkTok TokenColon }
  else                          { mkTok TokenElse }
  "="                           { mkTok TokenEq }
  false                         { mkTok TokenFalse }
  fun                           { mkTok TokenLambda }
  if                            { mkTok TokenIf }
  $digit+                       { mkTokWithInput (TokenInt . read) } -- <1>
  "{"                           { mkTok TokenLCurly }
  "("                           { mkTok TokenLParen }
  "<"                           { mkTok TokenLess }
  let                           { mkTok TokenLet }
  "-"                           { mkTok TokenMinus }
  "+"                           { mkTok TokenPlus }
  "}"                           { mkTok TokenRCurly }
  ")"                           { mkTok TokenRParen }
  \;\;                          { mkTok TokenSemiSemi }
  ","                           { mkTok TokenComma }
  then                          { mkTok TokenThen }
  "*"                           { mkTok TokenTimes }
  true                          { mkTok TokenTrue }
  "->"                          { mkTok TokenTypeArrow }
  bool                          { mkTok TokenTypeBool }
  int                           { mkTok TokenTypeInt }
  $alpha [$alpha $digit \_ \']* { mkTokWithInput TokenVar }        -- <2>
-- end::LexerTokens[]


{
data AlexUserState = AlexUserState FilePath


alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "[no file]"


getFilePath :: Alex FilePath
getFilePath = liftM (\(AlexUserState path) -> path) alexGetUserState


setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState


data Token = Token AlexPosn TokenName deriving (Eq, Show)


data TokenName
  = TokenColon
  | TokenComma
  | TokenDiv
  | TokenEOF
  | TokenElse
  | TokenEq
  | TokenFalse
  | TokenLambda
  | TokenIf
  | TokenInt Int
  | TokenLCurly
  | TokenLParen
  | TokenLess
  | TokenLet
  | TokenMinus
  | TokenPlus
  | TokenRCurly
  | TokenRParen
  | TokenSemiSemi
  | TokenThen
  | TokenTimes
  | TokenTrue
  | TokenTypeArrow
  | TokenTypeBool
  | TokenTypeInt
  | TokenVar String
  deriving (Eq, Show)


alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF


-- Depends on actual text (e.g. TokenVar, TokenInt)
mkTokWithInput :: (String -> TokenName) -> AlexAction Token
mkTokWithInput f = \(p,_,_,s) i -> return $ Token p (f (take i s))


-- Doesn't depend on the lexed text
mkTok :: TokenName -> AlexAction Token
mkTok = mkTokWithInput . const


alexScanAll :: Alex [Token]
alexScanAll = do
  tok <- alexMonadScan'
  case tok of
    (Token _ TokenEOF) -> return []
    x -> do
      xs <- alexScanAll
      return (x:xs)


alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF

    AlexError (p@(AlexPn _ line col), _, _, s) ->
      alexError' p
        (printf "%d:%d lexical error at '%s'" line col (take 1 s))

    AlexSkip inp' _ -> do
      alexSetInput inp'
      alexMonadScan'

    AlexToken inp' len action -> do
      alexSetInput inp'
      action (ignorePendingBytes inp) len


alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ line col) msg = do
  filePath <- getFilePath
  alexError (printf "%s: %d:%d: %s" filePath line col msg)


runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a filePath input = runAlex input (setFilePath filePath >> a)
}
