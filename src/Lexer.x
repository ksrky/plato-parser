{
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}

module Lexer where

import Monad
import Token
import Action
import Layout
import SrcLoc
import Error

import Control.Monad
import Control.Monad.State
}


$nl = [\n\r\f]
$white_nonl = $white # \n

$small = [a-z]
$large = [A-Z]
$alpha = [a-zA-Z]
$digit = 0-9

$special = [\(\)\,\;\[\]\`\{\}\_\"\']
$common = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$symbol = $common

@varid = $small [$alpha $digit \_ \']*
@conid = $large [$alpha $digit \_ \']*

@varsym = ($symbol # \:) $symbol*
@consym = \: $symbol+

@qual = (@conid \.)+
@qvarid = @qual @varid
@qconid = @qual @conid
@qvarsym = @qual @varsym
@qconsym = @qual @consym

@decimal = $digit+

tokens :-

<0> $nl+                        ;
<0> $white_nonl+                { spaces }
<code> $white_nonl+             ;
<code> $nl+                     { begin 0 }
<layout> $nl+                   ;
<layout> $white_nonl*           { layoutSpaces }

-- | line comment
<0, code> "--" \-* ~$symbol .*  ;

-- | block comment
<0, code, comment> "{-"         { beginComment }
<comment> "-}"                  { endComment }
<comment> $printable+           ;
<comment> $nl+                  ;


-- | keyword
<code> case                     { keyword KwCase }
<0, code> data                  { keyword KwData }
<code> forall                   { keyword KwForall }
<0, code> import                { keyword KwImport }
<0, code> infix                 { keyword KwInfix }
<0, code> infixl                { keyword KwInfixL }
<0, code> infixr                { keyword KwInfixR }
<code> in                       { keyword KwIn }
<code> of                       { layoutKeyword KwOf }
<code> let                      { layoutKeyword KwLet }
<0, code> module                { keyword KwModule }
<code> where                    { layoutKeyword KwWhere }

--| special symbol
<code> \,                       { symbol SymComma }
<code> \'                       { symbol SymDash }
<0, code> \{                    { leftBrace }
<code> \[                       { symbol SymLBrack }
<0, code> \(                    { symbol SymLParen }
<0, code> \}                    { rightBrace }
<code> \]                       { symbol SymRBrack }
<0, code> \)                    { symbol SymRParen }
<0, code> \;                    { symbol SymSemicolon }
<code> \_                       { symbol SymUScore }

<code> @varsym                  { varsym }

--| common symbol
<code> \-\>                     { symbol SymArrow }
<code> \\                       { symbol SymBackslash }
<code> \:                       { symbol SymColon }
<code> \=                       { symbol SymEqual }       
<code> \|                       { symbol SymVBar }

<0, code> @varid                { varid }
<0, code> @conid                { conid }
<code> @consym                  { consym }

<code> @qvarid                  { qvarid }
<code> @qconid                  { qconid }
<code> @qvarsym                 { qvarsym }
<code> @qconsym                 { qconsym }

<code> @decimal                 { integer }

{
lexer :: (Located Token -> Parser a) -> Parser a
lexer = (alexMonadScan >>=)

alexMonadScan :: Parser (Located Token)
alexMonadScan = do
    ainp@(pos, _, _, inp) <- getInput
    scd <- getStartCode
    lev <- getIndentLevels
    let sp = mkSpan pos inp 0
    case alexScan ainp scd of
        AlexEOF -> do
            cd <- getCommentDepth
            when (cd > 0) $ lift $ throwPsError sp "unterminated block comment"
            case lev of
                -- note: Layout rule
                -- L [] []                 = []
                -- L [] (m : ms)           = <closing brace>  :  L [] ms                     if m???0
                -- alex bug: closing brace inside a comment throws parse error
                [] -> return $ L sp TokEOF
                0 : _ -> lift $ throwPsError sp "closing brace missing"
                _ : ms -> do
                    setIndentLevels ms
                    ret sp (TokSymbol SymVRBrace)
        AlexError _ -> lift $ throwPsError sp "lexical error"
        AlexSkip ainp' _len -> do
            setInput ainp'
            alexMonadScan
        AlexToken ainp' len action -> do
            setInput ainp'
            action ainp len
}