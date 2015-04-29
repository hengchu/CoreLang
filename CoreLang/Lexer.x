{

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-|
 Module : CoreLang.Lexer
 Description : This module implements the lexer of CoreLang.
 Stability : Experimental
 -}
module CoreLang.Lexer
(
  Token(..)
, TokenClass(..)
, AlexPosn(..)
, scanner
, coreKeywords
) where

import Prelude hiding (LT, EQ, GT)
import Numeric (readDec)
import Control.Monad (when)
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
$whitespace = [\ \t]
$eol = [\n\r]

@number = [$digit]+
@ident  = $alpha($alpha|_|$digit)*

rules:-

<0>$whitespace ;
<0>$eol        {skip}

<0>"->"        {simpleToken RARROW}

<0>"+"         {simpleToken PLUS}
<0>"-"         {simpleToken MINUS}
<0>"*"         {simpleToken MULT}
<0>"/"         {simpleToken DIV}
<0>"<="        {simpleToken LE}
<0>"<"         {simpleToken LT}
<0>"=="        {simpleToken EQ}
<0>"~="        {simpleToken NEQ}
<0>">="        {simpleToken GE}
<0>">"         {simpleToken GT}
<0>"&"         {simpleToken AND}
<0>"|"         {simpleToken OR}

<0>"."         {simpleToken DOT}
<0>";"         {simpleToken SEMICOLON}
<0>"letrec"    {simpleToken LETREC}
<0>"let"       {simpleToken LET}
<0>"in"        {simpleToken IN}
<0>"case"      {simpleToken CASE}
<0>"of"        {simpleToken OF}
<0>\\          {simpleToken BACKSLASH}
<0>"("         {simpleToken LPAREN}
<0>")"         {simpleToken RPAREN}
<0>"Pack"      {simpleToken PACK}
<0>"{"         {simpleToken LBRAC}
<0>"}"         {simpleToken RBRAC}
<0>","         {simpleToken COMMA}
<0>"="         {simpleToken EQUAL}

<0>@ident      {mkIdent}
<0>@number     {mkNumber}

{

-- |Defines the set of keywords for CoreLang
coreKeywords :: [String]
coreKeywords =
  ["let", "letrec", "in", "case", "of", "Pack"]

type AlexUserState = ()

alexInitUserState :: AlexUserState
alexInitUserState = ()

data Token = Token AlexPosn TokenClass
  deriving (Show, Eq)

data TokenClass = SEMICOLON 
                | DOT       
                | LET       
                | LETREC    
                | IN        
                | CASE      
                | OF        
                | BACKSLASH 
                | LPAREN    
                | RPAREN    
                | PACK      
                | LBRAC     
                | RBRAC     
                | COMMA     
                | EQUAL     
                | RARROW    
                | NUMBER Int 
                | IDENT String 
                | PLUS      
                | MINUS     
                | MULT      
                | DIV       
                | LT        
                | LE        
                | EQ        
                | NEQ       
                | GE        
                | GT        
                | AND       
                | OR        
                | EOF       
  deriving (Show, Eq)

simpleToken :: TokenClass -> AlexInput -> Int -> Alex Token
simpleToken cls (p, _, _, _) _ = return (Token p cls)

mkIdent :: AlexInput -> Int -> Alex Token
mkIdent (p, _, _, str) len = return (Token p (IDENT s))
  where s = take len str

mkNumber :: AlexInput -> Int -> Alex Token
mkNumber (p, _, _, str) len = return (Token p (NUMBER d))
  where s = take len str
        d = fst $ head $ readDec s

alexEOF :: Alex Token
alexEOF = do (p, _, _, _) <- alexGetInput
             return (Token p EOF)

isEOF :: Token -> Bool
isEOF (Token _ EOF) = True
isEOF _ = False

-- |'scanner' function takes a string and returns either
-- an error or a list of 'Token's.
scanner :: String -> Either String [Token]
scanner str = 
  let loop = do t <- alexMonadScan
                if (isEOF t)
                then return [t]
                else do toks <- loop
                        return (t:toks)
  in  runAlex str loop

}
