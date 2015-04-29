{-|
 Module      : CoreLang.PPrint
 Description : This module implements a pretty print library for CoreLang.
 Stability   : Experimental
 -}
module CoreLang.PPrint
(
  -- * Pretty print function for CoreLang
  pprint
) where

import CoreLang.Language

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

-- = Beginning of pretty print code

type Precedence = Int

opsPrec :: [(String, Precedence)]
opsPrec =
  [("+", 4), ("-", 4), ("*", 5), 
   ("/", 5), ("<", 3), ("<=", 3), 
   ("==", 3), ("~=", 3), (">=", 3), 
   (">", 3), 
   ("&", 2), ("|", 1)]

pprExpr :: CoreExpr -> Precedence -> Iseq
pprExpr (EVar v) _ = iStr v
pprExpr (ENum n) _ = iStr $ show n
pprExpr (EAp (EAp (EVar op) e1) e2) ctxtprec | op `elem` (map fst opsPrec) =
  iConcat [ left, pprExpr e1 opprec, iStr (" " ++ op ++ " "), pprExpr e2 opprec, right ]
  where left = if ctxtprec >= opprec then iStr "(" else iNil
        right = if ctxtprec >= opprec then iStr ")" else iNil
        opprec = maybe (error "pprExpr: op has no associated precedence.") id prec
        prec = lookup op opsPrec

pprExpr (EAp e1 e2) prec = 
  iConcat [ left, (pprExpr e1 6), (iStr " "), (pprExpr e2 6), right ]
  where left = if prec >= 6 then iStr "(" else iNil
        right = if prec >= 6 then iStr ")" else iNil
pprExpr (ELet isrec defns expr) prec =
  iConcat [ left, iStr keyword, iNewline,
            iStr "  ", iIndent (pprDefns defns), iNewline,
            iStr "in ", pprExpr expr 0, right
          ]
  where keyword = if isrec then "letrec" else "let"
        left = if prec >= 6 then iStr "(" else iNil
        right = if prec >= 6 then iStr ")" else iNil
pprExpr (ELam names expr) prec = 
  iConcat $ [ left, iStr "\\" ] ++ map (iStr . (++" ")) names ++ [ iStr " . ", pprExpr expr 0, right ]
  where left = if prec >= 6 then iStr "(" else iNil
        right = if prec >= 6 then iStr ")" else iNil
pprExpr (EConstr n1 n2) _ =
  iStr $ "Pack{" ++ show n1 ++ "," ++ show n2 ++ "}"
pprExpr (ECase e1 alters) _ =
  iConcat [ iStr "(", iStr "case ", pprExpr e1 0, iStr " of", iNewline,
            iStr "  ", iIndent (pprAlts alters), iStr ")"
          ]

pprAlts :: [Alter Name] -> Iseq
pprAlts alts = iInterleave sep (map pprAlt alts)
  where sep = iConcat [iStr ";", iNewline]

pprAlt :: Alter Name -> Iseq
pprAlt (idx, vs, e) =
  iConcat [ iStr $ "<" ++ show idx ++ "> ",
            iConcat $ map (iStr . (++" ")) vs,
            iStr "-> ",
            pprExpr e 0
          ]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where sep = iConcat [iStr ";", iNewline]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) =
  iConcat [iStr name, iStr " = ", iIndent (pprExpr expr 0)]

space :: Int -> String
space n = take n $ repeat ' '

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

pprProgram :: CoreProgram -> Iseq
pprProgram scdefns = iInterleave sep $ map pprScDefn scdefns
  where sep = iConcat [ iStr ";", iNewline ]

pprScDefn :: CoreScDefn -> Iseq
pprScDefn (name, vars, expr) =
  iConcat [ iStr name, iStr " ",
            iInterleave (iStr " ") (map iStr vars),
            iStr " = ",
            pprExpr expr 0
          ]

-- = Pretty Print function exposed

-- |The 'pprint' function pretty prints a 'CoreProgram' given its
-- AST.
pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)
