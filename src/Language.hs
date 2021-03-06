{-# OPTIONS_GHC -Wall #-}
module Language where

import Prelude hiding (seq)

data Expr a
    = EVar Name             -- Variables
    | ENum Int              -- Numbers
    | EAp (Expr a) (Expr a) -- Applications
    | ELet                  -- Let(rec) expressions
        IsRec               --   boolean with True = recursive,
        [(a, Expr a)]       --   definitions
        (Expr a)            --   Body of let(rec)
    | ECase                 -- Case expressions
        (Expr a)            --   Expression to scrutinise
        [Alter a]           --   Alternatives
    | ELam [a] (Expr a)     -- Lambda abstractions
    deriving (Show)

type CoreExpr = Expr Name

type Name = String

type IsRec = Bool

recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

type Alter a = (Int,        -- Tag
                [a],        -- List of bound variables
                Expr a)     -- Right hand side of arrow

type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr EVar{} = True
isAtomicExpr ENum{} = True
isAtomicExpr _      = False

type Program a = [ScDefn a]

type CoreProgram = Program Name

type ScDefn a = (Name,      -- Name of the supercombinator
                 [a],       -- Arguments
                 Expr a)    -- Body

type CoreScDefn = ScDefn Name

preludeDefs :: CoreProgram
preludeDefs =
    -- I x = x ;
    [ ("I", ["x"], EVar "x")
    -- K x y = x ;
    , ("K", ["x", "y"], EVar "x")
    -- K1 x y = y ;
    , ("K1", ["x", "y"], EVar "y")
    -- S f g x = f x (g x) ;
    , ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                                 (EAp (EVar "g") (EVar "x")))
    -- compose f g x = f (g x) ;
    , ("compose", ["f", "g", "x"], EAp (EVar "f")
                                       (EAp (EVar "g") (EVar "x")))
    -- twice f = compose f f
    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    ]
