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

qconid                          { (mkLQConId -> Just $$) }

int                             { (mkLInt -> Just $$) }

%%

program     : 'module' modid ';' body               { Program (Just $2) (fst $4) (snd $4) }
            | body                                  { Program Nothing (fst $1) (snd $1) }

body        : impdecls ';' topdecls                 { ($1, $3) }
            | topdecls                              { ([], $1) }

impdecls    :: { [Located ImpDecl] }
            : impdecls ';' impdecl                  { $3 : $1 }
			| impdecl								{ [$1] }
            | {- empty -}                           { [] }

impdecl     :: { Located ImpDecl }
            : 'import' modid                        { sLL (unitLoc $1) $2 (ImpDecl $2) }

modid       :: { Located ModuleName }
            : qconid                                { sL $1 (ModuleName (splitModid $1)) }
            | conid                                 { sL $1 (ModuleName (splitModid $1)) }

topdecls    :: { [Located TopDecl] }
            : topdecl ';' topdecls                  { $1 : $3 }
            | {- empty -}                           { [] }

topdecl     :: { Located TopDecl }
            : 'data' conid tyvars '=' constrs       { sLLn (unitLoc $1) (snd $ last $5) (DataDecl (id2tyconName $2) $3 $5) }
            | 'data' conid tyvars                   { sLLn (unitLoc $1) $3 (DataDecl (id2tyconName $2) $3 []) }
            | conid tyvars '=' type                 { sLL $1 $4 (TypeDecl (id2tyconName $1) $2 $4) }
            | fixdecl                               { $1 }
            | decl                                  { sL $1 (Decl $1) }

fixdecl     :: { Located TopDecl }
            : 'infix' int op                        {% setFixity $3 $2 Nonfix >> return (L $1 FixDecl) }
            | 'infixl' int op                       {% setFixity $3 $2 Leftfix >> return (L $1 FixDecl) }
            | 'infixr' int op                       {% setFixity $3 $2 Rightfix >> return (L $1 FixDecl) }

decls       :: { [Located Decl] }
            : decl ';'                              { [$1] }
            | decl ';' decls                        { $1 : $3 }

decl        :: { Located Decl }
            : varid ':' type                        { sLL $1 $3 (FuncTyDecl (id2varName $1) $3) }
            | '(' varsym ')' ':' type               { sLL (unitLoc $1) $5 (FuncTyDecl (id2varName $2) $5) }
            | varid vars '=' expr                   { sLL $1 $4 (FuncDecl (id2varName $1) $2 $4) }
            | '(' varsym ')' vars '=' expr          { sLL (unitLoc $1) $6 (FuncDecl (id2varName $2) $4 $6) }

types       :: { [Located Type] }
            : atype types                           { $1 : $2 }
            | {- empty -}                           { [] }

type        :: { Located Type }
            : 'forall' varid tyvars '.' type        { sLL (unitLoc $1) $5 (AllType (id2tyvarName $2 : $3) $5) }
            | '{' varid tyvars '}' '->' type        { sLL (unitLoc $1) $6 (AllType (id2tyvarName $2 : $3) $6) }
            | btype '->' type                       { sLL $1 $3 (ArrType $1 $3) }
            | '(' type ')'                          { $2 }
            | btype                                 { $1 }

btype       :: { Located Type }
            : btype atype                           { sLL $1 $2 (AppType $1 $2) }
            | atype                                 { $1 }

atype       :: { Located Type }
            : '(' type ')'                          { $2 }
            | conid                                 { sL $1 (ConType (id2tyconName $1)) }
            | varid                                 { sL $1 (VarType (id2tyvarName $1)) }

constrs     :: { [(Located Name, [Located Type])] }
            : constr '|' constrs                    { $1 : $3 }
            | constr                                { [$1] }

constr      :: { (Located Name, [Located Type]) }
            : conid types                           { (id2conName $1, $2) }
            | '(' consym ')' types                  { (id2conName $2, $4) }

tyvars      :: { [Located Name] }
            : varid tyvars                          { id2tyvarName $1 : $2 }
            | {- empty -}                           { [] }

op          :: { Located Name }
            : varsym                                { id2varName $1 }
            | consym                                { id2conName $1 }
        
expr        :: { Located Expr }
            : infixexpr                             { $1 }

infixexpr   :: { Located Expr }
            : lexpr op infixexpr                    { sLL $1 $3 (OpExpr $1 $2 $3) }
            | lexpr                                 { $1 }

lexpr       :: { Located Expr }
            : '\\' varid vars '->' expr             { sLL (unitLoc $1) $5 (LamExpr (id2varName $2 : $3) $5) }
            | 'let' '{' decls '}' 'in' expr         { sLL (unitLoc $1) $6 (LetExpr $3 $6) }
            | 'case' expr 'of' '{' alts '}'         { L (combineSpans $1 $6) (CaseExpr $2 $5) }
            | fexpr                                 { $1 }

fexpr       :: { Located Expr }
            : fexpr aexpr                           { sLL $1 $2 (AppExpr $1 $2) }
            | aexpr                                 { $1 }

aexpr       :: { Located Expr }
            : '(' expr ')'                          { L (combineSpans $1 $3) (Factor $2) }
            | varid                                 { sL $1 (VarExpr (id2varName $1)) }
            | conid                                 { sL $1 (VarExpr (id2conName $1)) }

vars        :: { [Located Name] }
            : varid vars                            { id2varName $1 : $2 }
            | {- empty -}                           { [] }

alts        :: { [(Located Pat, Located Expr)] }
            : alt ';' alts                          { $1 : $3 }
            | {- empty -}                           { [] }

alt         :: { (Located Pat, Located Expr) }
            : pat '->' expr                         { ($1, $3) }

pat         :: { Located Pat }
            : conid apats                           { sLLn $1 $2 (ConPat (id2conName $1) $2) }
            | apat consym apats                     { sLLn $1 $3 (ConPat (id2conName $2) ($1 : $3)) }
            | apat                                  { $1 }

apats       :: { [Located Pat] }
            : apat apats                            { $1 : $2 }
            | apat                                  { [$1] }

apat        :: { Located Pat }
            : '(' pat ')'                         	{ $2 }
            | conid                               	{ sL $1 (ConPat (id2conName $1) []) }
            | varid                                 { sL $1 (VarPat (id2varName $1)) }
            | '_'                                   { L $1 WildPat }

{
splitModid :: Located T.Text -> [Name]
splitModid = loop 0 . ((`T.snoc` '.') <$>)
  where
    loop :: Int -> Located T.Text -> [Name]
    loop _ (L _ t) | null (T.unpack t) = []
    loop cnt (L sp t) =
        let xs = T.unpack t
         in if (xs !! cnt) == '.'
            then conName (T.take cnt t) : loop 0 (L sp (T.drop (cnt + 1) t))
            else loop (cnt + 1) (L sp t)

sL :: Located a -> b -> Located b
sL loc = L (getSpan loc)

sLL :: Located a -> Located b -> c -> Located c
sLL loc1 loc2 = L (combineSpans (getSpan loc1) (getSpan loc2))

sLLn :: Located a -> [Located b] -> c -> Located c
sLLn loc locs = L (concatSpans (getSpan loc : map getSpan locs))

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
id2varName :: Located T.Text -> Located Name
id2varName (L sp t)= L sp (varName t)

id2conName :: Located T.Text -> Located Name
id2conName (L sp t)= L sp (conName t)

id2tyvarName :: Located T.Text -> Located Name
id2tyvarName (L sp t)= L sp (tyvarName t)

id2tyconName :: Located T.Text -> Located Name
id2tyconName (L sp t)= L sp (tyconName t)
}