{
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}

module Lexer where

import Monad
import Token
import Action
import SrcLoc
import Error

import Control.Monad
import Control.Monad.State
}


$tab = \t
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

<0> $white+                     ;

-- | line comment
<0> "--" ~$symbol .*            ;
<0> "--" \-+ ~$symbol .*        ;

-- | block comment
<0, comment> "{-"               { beginComment }
<comment> "-}"                  { endComment }
<comment> $printable+           ;
<comment> $nl+                  ;


-- | keyword
<0> case                        { keyword KwCase }
<0> data                        { keyword KwData }
<0> forall                      { keyword KwForall }
<0> import                      { keyword KwImport }
<0> infix                       { keyword KwInfix }
<0> infixl                      { keyword KwInfixL }
<0> infixr                      { keyword KwInfixR }
<0> in                          { keyword KwIn }
<0> of                          { keyword KwOf }
<0> let                         { keyword KwLet }
<0> module                      { keyword KwModule }
<0> where                       { keyword KwWhere }

--| special symbol
<0> \,                          { symbol SymComma }
<0> \'                          { symbol SymDash }
<0> \{                          { symbol SymLBrace }
<0> \[                          { symbol SymLBrack }
<0> \(                          { symbol SymLParen }
<0> \}                          { symbol SymRBrace }
<0> \]                          { symbol SymRBrack }
<0> \)                          { symbol SymRParen }
<0> \;                          { symbol SymSemicolon }
<0> \_                          { symbol SymUScore }

<0> @varsym                     { varsym }

--| common symbol
<0> \-\>                        { symbol SymArrow }
<0> \\                          { symbol SymBackslash }
<0> \:                          { symbol SymColon }
<0> \.                          { symbol SymDot }
<0> \=                          { symbol SymEqual }       
<0> \|                          { symbol SymVBar }

<0> @varid                      { varid }
<0> @conid                      { conid }
<0> @consym                     { consym }

<0> @qvarid                     { qvarid }
<0> @qconid                     { qconid }
<0> @qvarsym                    { qvarsym }
<0> @qconsym                    { qconsym }

<0> @decimal                    { integer }

{
lexer :: (Located Token -> Parser a) -> Parser a
lexer = (alexMonadScan >>=)

alexMonadScan :: Parser (Located Token) 
alexMonadScan = do
    ainp@(pos, _, _, inp) <- getInput
    scd <- getStartCode
    let sp = mkSpan pos inp 0
    case alexScan ainp scd of
        AlexEOF -> do
            cd <- getCommentDepth
            when (cd > 0) $ lift $ throwPsError sp "unterminated block comment"
            return $ L sp TokEOF
        AlexError _ -> lift $ throwPsError sp "lexical error"
        AlexSkip ainp' _len -> do
            setInput ainp'
            alexMonadScan
        AlexToken ainp' len action -> do
            setInput ainp'
            action ainp len

beginComment :: Action
beginComment _ _ = do
    cd <- getCommentDepth 
    setCommentDepth (cd + 1)
    setStartCode comment
    alexMonadScan

endComment :: Action
endComment (pos,_,_,inp) len = do
    depth <- getCommentDepth
    let sp = mkSpan pos inp len
    when (depth <= 0) $ lift $ throwPsError sp "block comment terminated without starting"
    setCommentDepth (depth - 1)
    when (depth == 1) $ setStartCode 0
    alexMonadScan
}