{-# LANGUAGE RankNTypes #-}

module Action where

import Error
import {-# SOURCE #-} Lexer
import Monad
import SrcLoc
import Token

import Control.Monad.State
import qualified Data.Text as T

type Action = AlexInput -> Int -> Parser (Located Token)

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
-- SrcLoc
----------------------------------------------------------------
mkLoc :: PsPosn -> Loc
mkLoc (PsPosn _ l c) = Loc l c

mkSpan :: PsPosn -> T.Text -> Int -> Span
mkSpan pos inp len = Span (mkLoc pos) (mkLoc $ movePosn pos inp len)

----------------------------------------------------------------
-- Token
----------------------------------------------------------------
-- note: Alex does not match empty text, so the additional rule specific to the case n=0 is needed
token :: (T.Text -> Token) -> Action
token f ainp@(pos, _, _, inp) len = do
        let sp = mkSpan pos inp len
            t = T.take len inp
        lev <- getIndentLevels
        scd <- getStartCode
        case lev of
                _ | scd == code -> ret sp (f t)
                m : ms
                        | m == 0 -> do
                                -- note: Layout rule
                                -- L (< 0 >: ts) (m : ms)  = ;  :  (L ts (m : ms))             if m = 0
                                setStartCode code
                                setInput ainp
                                ret sp (TokSymbol SymSemicolon)
                        | m > 0 -> do
                                -- note: Layout rule
                                -- L (< 0 >: ts) (m : ms)  = }  :  (L (< 0 >: ts) ms)          if m > 0
                                setIndentLevels ms
                                setInput ainp
                                ret sp (TokSymbol SymVRBrace)
                [] -> do
                        ---------- note: Layout rule
                        ---------- L (< 0 >: ts) []        = L ts []
                        setStartCode code
                        setInput ainp
                        ret sp (TokSymbol SymSemicolon)
                _ -> error "unreachable: negative indent level"

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
endComment (pos, _, _, inp) len = do
        depth <- getCommentDepth
        let sp = mkSpan pos inp len
        when (depth <= 0) $ lift $ throwPsError sp "block comment terminated without starting"
        setCommentDepth (depth - 1)
        when (depth == 1) $ setStartCode code
        alexMonadScan
