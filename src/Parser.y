{
{-# LANGUAGE ViewPatterns #-}

module Parser where

import Lexer
import Syntax
import Token
import Name
import Error
import SrcLoc
import Monad
import Helper
import Pretty

import qualified Data.Text as T
import Control.Monad.State
}

%name parser program
%tokentype { Located Token }
%monad { Parser } { (>>=) } { return }
%lexer { lexer } { L _ TokEOF }
%error { parseError }

%token

'case'                          { L $$ (TokKeyword KwCase) }
'data'                          { L $$ (TokKeyword KwData) }
'forall'                        { L $$ (TokKeyword KwForall) }
'import'                        { L $$ (TokKeyword KwImport) }
'infix'                         { L $$ (TokKeyword KwInfix) }
'infixl'                        { L $$ (TokKeyword KwInfixL) }
'infixr'                        { L $$ (TokKeyword KwInfixR) }
'in'                            { L $$ (TokKeyword KwIn) }
'of'                            { L $$ (TokKeyword KwOf) }
'module'                        { L $$ (TokKeyword KwModule) }
'let'                           { L $$ (TokKeyword KwLet) }
'where'                         { L $$ (TokKeyword KwWhere) }

'->'                            { L $$ (TokSymbol SymArrow) }
'@'                             { L $$ (TokSymbol SymAt) }
'\\'                            { L $$ (TokSymbol SymBackslash) }
','                             { L $$ (TokSymbol SymComma) }
':'                             { L $$ (TokSymbol SymColon) }
'\''                            { L $$ (TokSymbol SymDash) }
'.'                             { L $$ (TokSymbol SymDot) }
'='                             { L $$ (TokSymbol SymEqual) }
'{'                             { L $$ (TokSymbol SymLBrace) }
'['                             { L $$ (TokSymbol SymLBrack) }
'('                             { L $$ (TokSymbol SymLParen) }
'}'                             { L $$ (TokSymbol SymRBrace) }
']'                             { L $$ (TokSymbol SymRBrack) }
')'                             { L $$ (TokSymbol SymRParen) }
';'                             { L $$ (TokSymbol SymSemicolon) }
'_'                             { L $$ (TokSymbol SymUScore) }
'|'                             { L $$ (TokSymbol SymVBar) }

varid                           { (mkLVarId -> Just $$) }
conid                           { (mkLConId -> Just $$) }
varsym                          { (mkLVarSym -> Just $$) }
consym                          { (mkLConSym -> Just $$) }

int                             { (mkLInt -> Just $$) }

%%

program     : body                                  { Program Nothing (fst $1) (snd $1) }

body        : topdecls                              { ([], $1) }

topdecls    :: { [TopDecl] }
            : topdecl ';' topdecls                  { $1 : $3 }
            | {- empty -}                           { [] }

topdecl     :: { TopDecl }
            : 'data' conid tyvars '=' constrs       { DataDecl (id2tyconName $2) $3 $5 }
            | 'data' conid tyvars                   { DataDecl (id2tyconName $2) $3 [] }
            | conid tyvars '=' type                 { TypeDecl (id2tyconName $1) $2 $4 }
            | decl                                  { Decl $1 }

decls       :: { [Decl] }
            : decl ';'                              { [$1] }
            | decl ';' decls                        { $1 : $3 }

decl        :: { Decl }
            : varid ':' type                        { FuncTyDecl (id2varName $1) $3 }
            | '(' varsym ')' ':' type               { FuncTyDecl (id2varName $2) $5 }
            | varid vars '=' expr                   { FuncDecl (id2varName $1) $2 $4 }
            | '(' varsym ')' vars '=' expr          { FuncDecl (id2varName $2) $4 $6 }

types       :: { [Type] }
            : atype types                           { $1 : $2 }
            | {- empty -}                           { [] }

type        :: { Type }
            : 'forall' varid tyvars '.' type        { AllType (id2tyvarName $2 : $3) $5 }
            | '{' varid tyvars '}' '->' type        { AllType (id2tyvarName $2 : $3) $6 }
            | btype '->' type                       { ArrType $1 $3 }
            | '(' type ')'                          { $2 }
            | btype                                 { $1 }

btype       :: { Type }
            : btype atype                           { AppType $1 $2 }
            | atype                                 { $1 }

atype       :: { Type }
            : '(' type ')'                          { $2 }
            | conid                                 { ConType (id2tyconName $1) }
            | varid                                 { VarType (id2tyvarName $1) }

constrs     :: { [(LName, [Type])] }
            : constr '|' constrs                    { $1 : $3 }
            | constr                                { [$1] }

constr      :: { (LName, [Type]) }
            : conid types                           { (id2conName $1, $2) }
            | '(' consym ')' types                  { (id2conName $2, $4)}

tyvars      :: { [LName] }
            : varid tyvars                          { id2tyvarName $1 : $2 }
            | {- empty -}                           { [] }

expr        :: { Expr }
            : lexpr                                 { $1 }

attyargs    :: { [Type] }
            : '@' atype attyargs                    { $2 : $3 }
            | {- empty -}                           { [] }

lexpr       :: { Expr }
            : '\\' varid vars '->' expr             { LamExpr (id2varName $2 : $3) $5 }
            | 'let' '{' decls '}' 'in' expr         { LetExpr $3 $6 }
            | 'case' expr 'of' '{' alts '}'         { CaseExpr $2 $5 }
            | fexpr                                 { $1 }

fexpr       :: { Expr }
            : fexpr aexpr                           { AppExpr $1 $2 }
            | fexpr '@' atype                       { TAppExpr $1 $3 }
            | aexpr                                 { $1 }

aexpr       :: { Expr }
            : '(' expr ')'                          { $2 }
            | varid                                 { VarExpr (id2varName $1) }
            | conid                                 { VarExpr (id2conName $1) }

vars        :: { [LName] }
            : varid vars                            { id2varName $1 : $2 }
            | {- empty -}                           { [] }

alts        :: { [(Pat, Expr)] }
            : alt ';' alts                          { $1 : $3 }
            | {- empty -}                           { [] }

alt         :: { (Pat, Expr) }
            : pat '->' expr                         { ($1, $3) }

pat         :: { Pat }
            : conid apats                           { ConPat (id2conName $1) $2 }
            | apat consym apats                     { ConPat (id2conName $2) ($1 : $3) }
            | apat                                  { $1 }

apats        :: { [Pat] }
            : apat apats                            { $1 : $2 }
            | apat                                  { [$1] }

apat        :: { Pat }
            : {-'(' pat ')'                         { $2 }
            |-} conid                               { ConPat (id2conName $1) [] }
            | varid                                 { VarPat (id2varName $1) }
            | '_'                                   { WildPat }

{
parseError :: Located Token -> Parser a
parseError (L sp tok) = lift $ throwPsError sp $ "parse error at '" ++ pretty tok ++ "'"

mkLVarId :: Located Token -> Maybe (Located T.Text)
mkLVarId (L sp (TokVarId t)) = Just (L sp t)
mkLVarId _ = Nothing

mkLConId :: Located Token -> Maybe (Located T.Text)
mkLConId (L sp (TokConId t)) = Just (L sp t)
mkLConId _ = Nothing

mkLVarSym :: Located Token -> Maybe (Located T.Text)
mkLVarSym (L sp (TokVarSym t)) = Just (L sp t)
mkLVarSym _ = Nothing

mkLConSym :: Located Token -> Maybe (Located T.Text)
mkLConSym (L sp (TokConSym t)) = Just (L sp t)
mkLConSym _ = Nothing

mkLInt :: Located Token -> Maybe (Located Int)
mkLInt (L sp (TokInt n)) = Just (L sp n)
mkLInt _ = Nothing

id2varName :: Located T.Text -> LName
id2varName (L sp t)= L sp (varName t)

id2conName :: Located T.Text -> LName
id2conName (L sp t)= L sp (conName t)

id2tyvarName :: Located T.Text -> LName
id2tyvarName (L sp t)= L sp (tyvarName t)

id2tyconName :: Located T.Text -> LName
id2tyconName (L sp t)= L sp (tyconName t)
}