{-|
 Module      : CoreLang.ISeq
 Description : This module implements a tools for pretty printing.
 Stability   : Experimental
 -}
module CoreLang.ISeq
(
  Iseq
, iIndent
, iNewline
, iNil
, iStr
, iAppend
, iDisplay
, iConcat
, iInterleave
, iNum
, iFWNum
, iLayn
, space
) where

-- = Iseq ADT used for pretty printing

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline

iIndent :: Iseq -> Iseq
iIndent iseq = IIndent iseq

iNewline :: Iseq
iNewline = INewline

iNil :: Iseq
iNil = INil

iStr :: String -> Iseq
iStr str = IStr str

iAppend :: Iseq -> Iseq -> Iseq
iAppend seq1 seq2 = IAppend seq1 seq2

iDisplay :: Iseq -> String
iDisplay s = flatten 0 [(s, 0)]

iConcat :: [Iseq] -> Iseq
iConcat [] = iNil
iConcat (s:seqs) = s `iAppend` (iConcat seqs)

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave sep seqs = iConcat $ iInterleave' [] seqs
  where iInterleave' worklist [] = reverse (drop 1 worklist)
        iInterleave' worklist (s:ss) = iInterleave' (sep:s:worklist) ss

iNum :: Int -> Iseq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseq
iFWNum width n =
  iStr (space (width - length digits) ++ digits)
  where digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat $ map layItem (zip [1..] seqs)
  where layItem (n, iseq) = iConcat [ iFWNum 4 n, iStr ") ", iIndent iseq, iNewline ]

flatten :: Int                -- ^ Current column
            -> [(Iseq, Int)]  -- ^ Work list
            -> String
flatten _ ((INewline, indent) : seqs) =
  '\n' : (space indent) ++ (flatten indent seqs)
flatten col ((IIndent s, _) : seqs) =
  flatten col ((s, col) : seqs)
flatten col ((INil, _) : seqs) =
  flatten col seqs
flatten col ((IStr str, _) : seqs) =
  str ++ flatten (col+length str) seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) =
  flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten _ [] = ""

space :: Int -> String
space n = replicate n ' '
