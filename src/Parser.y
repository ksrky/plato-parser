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
import Fixity

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
qconid                          { (mkLQConId -> Just $$) }
varsym                          { (mkLVarSym -> Just $$) }
consym                          { (mkLConSym -> Just $$) }

int                             { (mkLInt -> Just $$) }

%%

program     : 'module' modid ';' body               { Program (Just $2) (fst $4) (snd $4) }
            | body                                  { Program Nothing (fst $1) (snd $1) }

body        : impdecls ';' topdecls                 { ($1, $3) }
            | topdecls                              { ([], $1) }

impdecls    :: { [ImpDecl] }
            : impdecl ';' impdecls                  { $1 : $3 }
            | {- empty -}                           { [] }

impdecl     :: { ImpDecl }
            : 'import' modid                        { ImpDecl $2 }

modid       :: { ModuleName }
            : qconid                                { ModuleName (splitModid $1) }
            | conid                                 { ModuleName (splitModid $1) }

topdecls    :: { [TopDecl] }
            : topdecl ';' topdecls                  { $1 : $3 }
            | {- empty -}                           { [] }

topdecl     :: { TopDecl }
            : 'data' conid tyvars '=' constrs       { DataDecl (id2tyconName $2) $3 $5 }
            | 'data' conid tyvars                   { DataDecl (id2tyconName $2) $3 [] }
            | conid tyvars '=' type                 { TypeDecl (id2tyconName $1) $2 $4 }
            | fixdecl                               {% $1 >> return FixDecl }
            | decl                                  { Decl $1 }

decls       :: { [Decl] }
            : decl ';'                              { [$1] }
            | decl ';' decls                        { $1 : $3 }

decl        :: { Decl }
            : varid ':' type                        { FuncTyDecl (id2varName $1) $3 }
            | '(' varsym ')' ':' type               { FuncTyDecl (id2varName $2) $5 }
            | varid vars '=' expr                   { FuncDecl (id2varName $1) $2 $4 }
            | '(' varsym ')' vars '=' expr          { FuncDecl (id2varName $2) $4 $6 }

fixdecl     :: { Parser () }
            : 'infix' int op                        { setFixity $3 $2 Nonfix }
            | 'infixl' int op                       { setFixity $3 $2 Leftfix }
            | 'infixr' int op                       { setFixity $3 $2 Rightfix }

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

op          :: { LName }
            : varsym                                { id2varName $1 }
            | consym                                { id2conName $1 }
        
expr        :: { Expr }
            : infixexpr                             { $1 }

infixexpr   :: { Expr }
            : lexpr op infixexpr                    { OpExpr $1 $2 $3 }
            | lexpr                                 { $1 }

lexpr       :: { Expr }
            : '\\' varid vars '->' expr             { LamExpr (id2varName $2 : $3) $5 }
            | 'let' '{' decls '}' 'in' expr         { LetExpr $3 $6 }
            | 'case' expr 'of' '{' alts '}'         { CaseExpr $2 $5 }
            | fexpr                                 { $1 }

fexpr       :: { Expr }
            : fexpr aexpr                           { AppExpr $1 $2 }
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
splitModid :: Located T.Text -> [Name]
splitModid = loop 0 . ((`T.snoc` '.') <$>)
  where
    loop :: Int -> Located T.Text -> [Name]
    loop cnt (L sp t) =
        let xs = T.unpack t
         in if (xs !! cnt) == '.'
            then conName (T.take cnt t) : loop 0 (L sp (T.drop (cnt + 1) t))
            else loop (cnt + 1) (L sp t)
 
----------------------------------------------------------------
-- mkLocated
----------------------------------------------------------------
mkLVarId :: Located Token -> Maybe (Located T.Text)
mkLVarId (L sp (TokVarId t)) = Just (L sp t)
mkLVarId _ = Nothing

mkLConId :: Located Token -> Maybe (Located T.Text)
mkLConId (L sp (TokConId t)) = Just (L sp t)
mkLConId _ = Nothing

mkLQConId :: Located Token -> Maybe (Located T.Text)
mkLQConId (L sp (TokQConId t)) = Just (L sp t)
mkLQConId _ = Nothing

mkLVarSym :: Located Token -> Maybe (Located T.Text)
mkLVarSym (L sp (TokVarSym t)) = Just (L sp t)
mkLVarSym _ = Nothing

mkLConSym :: Located Token -> Maybe (Located T.Text)
mkLConSym (L sp (TokConSym t)) = Just (L sp t)
mkLConSym _ = Nothing

mkLInt :: Located Token -> Maybe (Located Int)
mkLInt (L sp (TokInt n)) = Just (L sp n)
mkLInt _ = Nothing

----------------------------------------------------------------
-- id2Name
----------------------------------------------------------------
id2varName :: Located T.Text -> LName
id2varName (L sp t)= L sp (varName t)

id2conName :: Located T.Text -> LName
id2conName (L sp t)= L sp (conName t)

id2tyvarName :: Located T.Text -> LName
id2tyvarName (L sp t)= L sp (tyvarName t)

id2tyconName :: Located T.Text -> LName
id2tyconName (L sp t)= L sp (tyconName t)
}