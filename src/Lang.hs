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

type Ident = String

type IsRec = Bool

type Program a = [ScDefn a]

type CoreProgram = Program Ident

type ScDefn a = (Ident, [a], Expr a)

type CoreScDefn = ScDefn Ident

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
prettyPrint [] = ""
prettyPrint ((name, args, body) : rest) =
  name ++ " " ++ unwords args ++ " = " ++ showExpr body ++ "\n" ++ prettyPrint rest
    where
        showExpr (EVar x) = x
        showExpr (ENum n) = show n
        showExpr (ECon n m) = "C" ++ show n ++ " " ++ show m
        showExpr (EApp e1 e2) = "(" ++ showExpr e1 ++ " " ++ showExpr e2 ++ ")"
        showExpr (ELet isRec defs body) =
            let name = if isRec then "letrec" else "let"
                defStr = unwords [x ++ " = " ++ showExpr e | (x, e) <- defs]
                in name ++ " " ++ defStr ++ " in " ++ showExpr body
        showExpr (ECase e alts) =
            let altStr = unwords [showAlt (n, xs, e) | (n, xs, e) <- alts]
                showAlt (n, xs, e) =
                    let argStr = unwords xs
                        in "C" ++ show n ++ " " ++ argStr ++ " -> " ++ showExpr e
                in "case " ++ showExpr e ++ " of " ++ altStr
        showExpr (ELam xs e) =
            let argStr = unwords xs
                in "(\\" ++ argStr ++ " -> " ++ showExpr e ++ ")"
