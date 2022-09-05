module Syntax where

import Name
import SrcLoc

type LName = Located Name

data Expr
        = VarExpr LName
        | AppExpr Expr Expr
        | OpExpr Expr LName Expr
        | LamExpr [LName] Expr
        | LetExpr [Decl] Expr
        | CaseExpr Expr [(Pat, Expr)]
        deriving (Eq, Show)

data Pat
        = ConPat LName [Pat]
        | VarPat LName
        | WildPat
        deriving (Eq, Show)

data Type
        = ConType LName
        | VarType LName
        | AppType Type Type
        | ArrType Type Type
        | AllType [LName] Type
        deriving (Eq, Show)

data Decl
        = FuncDecl LName [LName] Expr
        | FuncTyDecl LName Type
        deriving (Eq, Show)

data TopDecl
        = DataDecl LName [LName] [(LName, [Type])]
        | TypeDecl LName [LName] Type
        | Decl Decl
        | FixDecl
        deriving (Eq, Show)

newtype ImpDecl = ImpDecl ModuleName deriving (Eq, Show)

data Program = Program
        { moduleDecl :: Maybe ModuleName
        , importDecls :: [ImpDecl]
        , topDecls :: [TopDecl]
        }
        deriving (Show)
