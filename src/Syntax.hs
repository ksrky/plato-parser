module Syntax where

import Name
import SrcLoc

type LName = Located Name
type LExpr = Located Expr
type LPat = Located Pat
type LType = Located Type
type LDecl = Located Decl

data Expr
        = VarExpr LName
        | AppExpr LExpr LExpr
        | OpExpr LExpr LName LExpr
        | LamExpr [LName] LExpr
        | LetExpr [LDecl] LExpr
        | CaseExpr LExpr [(LPat, LExpr)]
        | Factor LExpr -- removed after fixity resolution
        deriving (Eq, Show)

data Pat
        = ConPat LName [LPat]
        | VarPat LName
        | WildPat
        deriving (Eq, Show)

data Type
        = ConType LName
        | VarType LName
        | AppType LType LType
        | ArrType LType LType
        | AllType [LName] LType
        deriving (Eq, Show)

data Decl
        = FuncDecl LName [LName] LExpr
        | FuncTyDecl LName LType
        deriving (Eq, Show)

data TopDecl
        = DataDecl LName [LName] [(LName, [LType])]
        | TypeDecl LName [LName] LType
        | Decl LDecl
        | FixDecl
        deriving (Eq, Show)

newtype ImpDecl = ImpDecl (Located ModuleName) deriving (Eq, Show)

data Program = Program
        { moduleDecl :: Maybe (Located ModuleName)
        , importDecls :: [Located ImpDecl]
        , topDecls :: [Located TopDecl]
        }
        deriving (Show)
