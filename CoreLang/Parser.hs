{-|
 Module : CoreLang.Parser
 Description : This module implements a parser for the Core language.
 Stability : Experimental
 -}
module CoreLang.Parser
(
  -- * Parsing function for Core language
  parseCoreProg
) where

import Text.Parsec
import Text.Parsec.Expr
import Data.Functor.Identity

import CoreLang.Lexer as L
import CoreLang.Language as A

type Parser = ParsecT [L.Token] () Identity

updatePos :: SourcePos -> L.Token -> [L.Token] -> SourcePos
updatePos pos (L.Token (L.AlexPn _ line col) _) _ =
  setSourceLine (setSourceColumn pos col) line

parseSimpleToken :: L.TokenClass -> Parser L.Token
parseSimpleToken cls = tokenPrim show updatePos acceptTok
  where acceptTok t@(L.Token _ c) | c == cls = Just t
                                  | otherwise = Nothing

parseId :: Parser (L.Token, A.Name)
parseId = tokenPrim show updatePos acceptTok
  where acceptTok idtok@(L.Token _ (L.IDENT name)) = Just (idtok, name)
        acceptTok _ = Nothing

parseNumber :: Parser (L.Token, Int)
parseNumber = tokenPrim show updatePos acceptTok
  where acceptTok numtok@(L.Token _ (L.NUMBER num)) = Just (numtok, num)
        acceptTok _ = Nothing

pVar :: Parser A.CoreExpr
pVar = do
  (_, name) <- parseId
  return $ A.EVar name

pNum :: Parser A.CoreExpr
pNum = do
  (_, num) <- parseNumber
  return $ A.ENum num

pConstr :: Parser A.CoreExpr
pConstr = do
  parseSimpleToken L.PACK
  parseSimpleToken L.LBRAC
  (_, n1) <- parseNumber
  parseSimpleToken L.COMMA
  (_, n2) <- parseNumber
  parseSimpleToken L.RBRAC
  return $ A.EConstr n1 n2

pParenE :: Parser A.CoreExpr
pParenE = do
  between (parseSimpleToken L.LPAREN) (parseSimpleToken L.RPAREN) expr

pAexpr :: Parser A.CoreExpr
pAexpr = try pVar
     <|> try pNum
     <|> try pConstr
     <|> pParenE
     <?> "aexpr"

pApOrAexpr :: Parser A.CoreExpr
pApOrAexpr = do
  aexprs <- many1 pAexpr
  if length aexprs > 1
     then return $ foldl1 EAp aexprs
     else return $ aexprs !! 0

type Defn = (A.Name, A.CoreExpr)

pLetCommon :: Parser ([Defn], A.CoreExpr)
pLetCommon = do
  defns <- pDefns
  parseSimpleToken L.IN
  e <- expr
  return (defns, e)

pLet :: Parser A.CoreExpr
pLet = do
  parseSimpleToken L.LET
  (defns, e) <- pLetCommon
  return $ A.ELet A.nonRecursive defns e

pLetRec :: Parser A.CoreExpr
pLetRec = do
  parseSimpleToken L.LETREC
  (defns, e) <- pLetCommon
  return $ A.ELet A.recursive defns e

pCase :: Parser A.CoreExpr
pCase = do
  parseSimpleToken L.CASE
  e <- expr
  parseSimpleToken L.OF
  alts <- pAlts
  return $ A.ECase e alts

pLam :: Parser A.CoreExpr
pLam = do
  parseSimpleToken L.BACKSLASH
  vars <- many1 parseId
  parseSimpleToken L.DOT
  e <- expr
  return $ A.ELam (map snd vars) e

pExprTerm :: Parser A.CoreExpr
pExprTerm = try pApOrAexpr
        <|> try pLetRec
        <|> try pLet
        <|> try pCase
        <|> try pLam
        <|> pAexpr
        <?> "exp-term"

pDefns :: Parser [Defn]
pDefns = pDefn `sepBy1` (parseSimpleToken L.SEMICOLON)

pDefn :: Parser Defn
pDefn = do
  (_, name) <- parseId
  parseSimpleToken L.EQUAL
  e <- expr
  return (name, e)

pAlts :: Parser [A.CoreAlt]
pAlts = pAlt `sepBy1` (parseSimpleToken L.SEMICOLON)

pAlt :: Parser A.CoreAlt
pAlt = do
  parseSimpleToken L.LT
  (_, n) <- parseNumber
  parseSimpleToken L.GT
  vars <- many parseId
  parseSimpleToken L.RARROW
  e <- expr
  return (n, (map snd vars), e)

pProgram :: Parser A.CoreProgram
pProgram = pSc `sepBy1` (parseSimpleToken L.SEMICOLON)

pSc :: Parser A.CoreScDefn
pSc = do
  vars <- many1 parseId
  let names = map snd vars
  parseSimpleToken L.EQUAL
  e <- expr
  return (head names, tail names, e)

binary :: ParsecT s u m t -> (a -> a -> a) -> Assoc -> Operator s u m a
binary parseOp fun = Infix (try $ do { parseOp; return fun })

buildBinopExpr :: String -> CoreExpr -> CoreExpr -> CoreExpr
buildBinopExpr op e1 e2 =
  A.EAp (A.EAp (EVar op) e1) e2

operatorTable :: [[Operator [Token] () Identity CoreExpr]]
operatorTable =
  [
    [ binary (parseSimpleToken L.MULT) (buildBinopExpr "*") AssocRight
    , binary (parseSimpleToken L.DIV)  (buildBinopExpr "/") AssocNone ]
  , [ binary (parseSimpleToken L.PLUS) (buildBinopExpr "+") AssocRight
    , binary (parseSimpleToken L.MINUS) (buildBinopExpr "-") AssocNone ]
  , [ binary (parseSimpleToken L.EQ)   (buildBinopExpr "==") AssocNone
    , binary (parseSimpleToken L.NEQ)  (buildBinopExpr "~=") AssocNone
    , binary (parseSimpleToken L.GE)   (buildBinopExpr ">=") AssocNone
    , binary (parseSimpleToken L.GT)   (buildBinopExpr ">")  AssocNone
    , binary (parseSimpleToken L.LE)   (buildBinopExpr "<=") AssocNone
    , binary (parseSimpleToken L.LT)   (buildBinopExpr "<")  AssocNone ]
  , [ binary (parseSimpleToken L.AND)  (buildBinopExpr "&") AssocRight ]
  , [ binary (parseSimpleToken L.OR)   (buildBinopExpr "|") AssocRight ]
  ]

expr :: Parser A.CoreExpr
expr = buildExpressionParser operatorTable pExprTerm

-- |'parseCoreProg' takes a string, and calles out to 'scanner'
-- to turn string into tokens, and then parses the tokens into
-- a 'CoreProgram'.
-- However, if the lexer fails, an exception is raised with
-- 'error'.
parseCoreProg :: String -> Either ParseError A.CoreProgram
parseCoreProg str =
  let eithertokens = L.scanner str
  in  case eithertokens of
        Left err -> error $ "Lexer error: " ++ err
        Right toks -> runIdentity (runPT pProgram () "<input>" toks)
