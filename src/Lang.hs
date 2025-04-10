{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lang
  ( Ident,
    IsRec,
    Program,
    CoreProgram,
    ScDefn,
    CoreScDefn,
    Expr (..),
    bindersOf,
    rhssOf,
    isAtomExpr,
    preludeDefs,
    prettyPrint
  )
where
import Data.List (intercalate)

type Ident = String

type IsRec = Bool

type Program a = [ScDefn a]

type CoreProgram = Program Ident

type ScDefn a = (Ident, [a], Expr a)

type CoreScDefn = ScDefn Ident

type CoreExpr = Expr Ident

data Expr a
  = EVar Ident
  | ENum Int
  | ECon Int Int
  | EApp (Expr a) (Expr a)
  | ELet IsRec [(Ident, Expr a)] (Expr a)
  | ECase (Expr a) [(Int, [Ident], Expr a)]
  | ELam [a] (Expr a)
  deriving (Show, Eq)

bindersOf :: [(Ident, Expr a)] -> [Ident]
bindersOf defns = [x | (x, _) <- defns]

rhssOf :: [(Ident, Expr a)] -> [Expr a]
rhssOf defns = [e | (_, e) <- defns]

isAtomExpr :: Expr a -> Bool
isAtomExpr (EVar _) = True
isAtomExpr (ENum _) = True
isAtomExpr _ = False

preludeDefs :: CoreProgram
preludeDefs =
  [ ("I", ["x"], EVar "x"),
    ("K", ["x", "y"], EVar "x"),
    ("K1", ["x", "y"], EVar "y"),
    ("S", ["f", "g", "x"], EApp (EApp (EVar "f") (EVar "x")) (EApp (EVar "g") (EVar "x"))),
    ("compose", ["f", "g", "x"], EApp (EVar "f") (EApp (EVar "g") (EVar "x"))),
    ("twice", ["f", "x"], EApp (EVar "f") (EApp (EVar "f") (EVar "x")))
  ]

prettyPrint :: CoreProgram -> String
prettyPrint = unlines . map formatScDef
    where
        formatScDef (name, args, body) = 
            name ++ " " ++ unwords args ++ " = " ++ showExpr body

        showExpr (EVar x) = x
        showExpr (ENum n) = show n
        showExpr (ECon tag arity) = "Pack{" ++ show tag ++ "," ++ show arity ++ "}"
        showExpr (EApp e1 e2) = showExpr e1 ++ " " ++ showAtom e2
        showExpr (ELet isRec defs body) =
            let keyword = if isRec then "letrec" else "let"
                defStrs = map formatDef defs
            in keyword ++ " " ++ intercalate "; " defStrs ++ " in " ++ showExpr body
        showExpr (ECase e alts) =
            "case " ++ showExpr e ++ " of " ++ 
            intercalate "; " (map formatAlt alts)
        showExpr (ELam args body) = 
            "(\\" ++ unwords args ++ " -> " ++ showExpr body ++ ")"

        formatDef (name, expr) = name ++ " = " ++ showExpr expr
        
        formatAlt (tag, args, expr) = 
            "<" ++ show tag ++ "> " ++ unwords args ++ " -> " ++ showExpr expr
        
        showAtom (EVar x) = x
        showAtom (ENum n) = show n
        showAtom e = "(" ++ showExpr e ++ ")"

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EApp e1 (replicate n e2)
