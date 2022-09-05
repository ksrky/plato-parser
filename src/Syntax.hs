module Syntax where

import Name
import SrcLoc

type PsLName = Located Name
type PsLExpr = Located Expr
type PsLPat = Located Pat
type PsLType = Located Type
type PsLDecl = Located Decl

data Expr
        = VarExpr PsLName
        | AppExpr PsLExpr PsLExpr
        | OpExpr PsLExpr PsLName PsLExpr
        | LamExpr [PsLName] PsLExpr
        | LetExpr [PsLDecl] PsLExpr
        | CaseExpr PsLExpr [(PsLPat, PsLExpr)]
        | Factor PsLExpr -- removed after fixity resolution
        deriving (Eq, Show)

data Pat
        = ConPat PsLName [PsLPat]
        | VarPat PsLName
        | WildPat
        deriving (Eq, Show)

data Type
        = ConType PsLName
        | VarType PsLName
        | AppType PsLType PsLType
        | ArrType PsLType PsLType
        | AllType [PsLName] PsLType
        deriving (Eq, Show)

data Decl
        = FuncDecl PsLName [PsLName] PsLExpr
        | FuncTyDecl PsLName PsLType
        deriving (Eq, Show)

data TopDecl
        = DataDecl PsLName [PsLName] [(PsLName, [PsLType])]
        | TypeDecl PsLName [PsLName] PsLType
        | Decl PsLDecl
        | FixDecl
        deriving (Eq, Show)

newtype ImpDecl = ImpDecl ModuleName deriving (Eq, Show)

data Program = Program
        { moduleDecl :: Maybe ModuleName
        , importDecls :: [ImpDecl]
        , topDecls :: [Located TopDecl]
        }
        deriving (Show)
