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
import Data.Text as T
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

<0> $nl+                        ;
<0> $white_nonl+                { spaces }
<code> $white_nonl+             ;
<code> $nl+                     { begin 0 }
<layout> $nl+                   ;
<layout> $white_nonl*           { layoutSpaces }

-- | line comment
<0, code> "--" \-* ~$symbol .*        ;

-- | block comment
<0, code, comment> "{-"               { beginComment }
<comment> "-}"                  { endComment }
<comment> $printable+           ;
<comment> $nl+                  ;


-- | keyword
<code> case                     { keyword KwCase }
<0, code> data                        { keyword KwData }
<code> forall                   { keyword KwForall }
<0, code> import                      { keyword KwImport }
<0, code> infix                       { keyword KwInfix }
<0, code> infixl                      { keyword KwInfixL }
<0, code> infixr                      { keyword KwInfixR }
<code> in                       { keyword KwIn }
<code> of                       { layoutKeyword KwOf }
<code> let                      { layoutKeyword KwLet }
<0, code> module                      { keyword KwModule }
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

----------------------------------------------------------------
-- Combinators
----------------------------------------------------------------
skip :: Action
skip _ _ = alexMonadScan

andBegin :: Action -> Int -> Action
(act `andBegin` code) ainp len = do
    setStartCode code
    act ainp len

begin :: Int -> Action
begin code = skip `andBegin` code

ret :: Span -> Token -> Parser (Located Token)
ret sp t = do
    addPrevTokens t
    return $ L sp t

----------------------------------------------------------------
-- Token
----------------------------------------------------------------
token :: (T.Text -> Token) -> Action
token f ainp@(pos, _, _, inp) len = do
    let sp = mkSpan pos inp len
        t = T.take len inp
    il <- getIndentLevels
    scd <- getStartCode
    case il of
        _ | scd == code -> ret sp (f t)
        m : ms
            | m == 0 -> do
                setStartCode code
                setInput ainp
                ret sp (TokSymbol SymSemicolon)
            | m > 0 -> do
                setStartCode 0
                setIndentLevels ms
                setInput ainp
                ret sp (TokSymbol SymVRBrace)
        [] -> do
            setStartCode code
            setInput ainp
            ret sp (TokSymbol SymSemicolon)
        _ -> error "unreachable: negative indent level"
    {- if scd == 0
        then do
            setStartCode code
            setInput ainp
            ret (mkSpan pos inp 0) (TokSymbol SymSemicolon)
        else do
            ret sp (f t) -}

keyword :: Keyword -> Action
keyword = token . const . TokKeyword

symbol :: Symbol -> Action
symbol = token . const . TokSymbol

varid :: Action
varid = token TokVarId

conid :: Action
conid = token TokConId

varsym :: Action
varsym (pos, _, _, inp) len = do
    let sp = mkSpan pos inp len
        t = T.take len inp
    case lookup t commonSymbols of
        Just sym -> return $ L sp (TokSymbol sym)
        Nothing -> return $ L sp (TokVarSym t)

consym :: Action
consym = token TokConSym

qvarid :: Action
qvarid = token TokQVarId

qconid :: Action
qconid = token TokQConId

qvarsym :: Action
qvarsym = token TokQVarSym

qconsym :: Action
qconsym = token TokQConSym

integer :: Action
integer = token (TokInt . read . T.unpack)

----------------------------------------------------------------
-- Comment
----------------------------------------------------------------
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
    when (depth == 1) $ setStartCode code
    alexMonadScan

----------------------------------------------------------------
-- Layout
----------------------------------------------------------------
layoutKeyword :: Keyword -> Action
layoutKeyword key (pos, _, _, inp) len = do
    let sp = mkSpan pos inp len
    setStartCode layout
    ret sp (TokKeyword key)

spaces :: Action
spaces ainp@(pos, _, _, inp) len = do
    il <- getIndentLevels
    let sp = mkSpan pos inp 0
    case il of
        m : ms
            | m == len -> do
                setStartCode code
                setIndentLevels (m : ms)
                ret sp (TokSymbol SymSemicolon)
            | m > len -> do
                setStartCode 0
                setInput ainp
                setIndentLevels ms
                ret sp (TokSymbol SymVRBrace)
        _ -> do
            setStartCode code
            alexMonadScan

layoutSpaces :: AlexInput -> Int -> Parser (Located Token)
layoutSpaces (pos, _, _, inp) len = do
    setStartCode code
    il <- getIndentLevels
    let sp = mkSpan pos inp 0
    case il of
        _ | T.unpack inp !! len == ';' -> alexMonadScan
        m : ms | len > m -> do
            setIndentLevels (len : m : ms)
            ret sp (TokSymbol SymVLBrace)
        [] | len > 0 -> do
            setIndentLevels [len]
            ret sp (TokSymbol SymVLBrace)
        _ -> ret sp (TokSymbol SymVLBrace) -- tmp:  L sp (TokSymbol SymVRBrace)]

pushSemicolon :: Action
pushSemicolon (pos, _, _, inp) _ = do
    setStartCode 0
    ret (mkSpan pos inp 0) (TokSymbol SymSemicolon)
}