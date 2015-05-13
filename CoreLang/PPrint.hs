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
import CoreLang.ISeq

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
