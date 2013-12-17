{-# OPTIONS_GHC -Wall #-}
module PPrint where

import Language

import Prelude hiding (seq)

--
-- efficient pretty-printing
--

iNil     :: Iseq            -- The empty iseq
iStr     :: String -> Iseq  -- Turn a string into an iseq
iAppend  :: Iseq -> Iseq -> Iseq -- Append two iseqs
iNewline :: Iseq            -- New line with indentation
iIndent  :: Iseq -> Iseq    -- Indent an iseq
iDisplay :: Iseq -> String  -- Turn an iseq into a string

iConcat :: [Iseq] -> Iseq
iConcat = foldl iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ []     = iNil
iInterleave _ [s]    = s
iInterleave sep seqs = foldl1 (\acc n -> acc `iAppend` sep `iAppend` n) seqs

data Iseq
    = INil
    | IStr String
    | IAppend Iseq Iseq
    | IIndent Iseq
    | INewline

iNil = INil

iAppend INil a = a
iAppend a INil = a
iAppend a b = IAppend a b

iStr = IStr

iIndent = IIndent

iNewline = INewline

flatten :: Int                  -- Current column; 0 for first column
            -> [(Iseq, Int)]    -- Work list (int is indentation required for Iseq)
            -> String           -- Result
flatten _ [] = ""
flatten _   ((INewline, indent) : seqs) = '\n' : space indent ++ flatten indent seqs
flatten col ((IIndent seq, _) : seqs) = flatten col ((seq, col) : seqs)
flatten col ((INil, _) : seqs) = flatten col seqs
flatten col ((IStr s, indent) : seqs)
  | indent > col = space indent ++ s ++ flatten (indent + length s) seqs
  | otherwise    = s ++ flatten (col + length s) seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) = flatten col ((seq1, indent) : (seq2, indent) : seqs)

space :: Int -> String
space i = replicate i ' '

iDisplay seq = flatten 0 [(seq, 0)]

iNum :: Int -> Iseq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseq
iFWNum width n = iStr (space (width - length digits) ++ digits)
  where
    digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
  where
    lay_item (n, seq) = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]



pprAExpr :: CoreExpr -> Iseq
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = iStr "(" `iAppend` pprExpr e `iAppend` iStr ")"

-- TODO: precedences and parens
pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = pprExpr e1 `iAppend` iStr " " `iAppend` pprAExpr e2
pprExpr (ELet isrec defns expr) =
    iConcat [ iStr keyword, iNewline,
              iStr " ", iIndent (pprDefns defns), iNewline,
              iStr "in ", pprExpr expr ]
  where
    keyword = if isrec then "letrec" else "let"
pprExpr (ENum n) = iNum n
pprExpr (ELam names body) = iConcat (iStr "(\\" : (map iStr names) ++ [iStr ". ", pprExpr body, iStr ")"])

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) =
    iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

pprProgram :: CoreProgram -> Iseq
pprProgram = undefined -- definition is not given in the book

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)
