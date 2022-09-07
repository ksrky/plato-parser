{-# LANGUAGE RankNTypes #-}

module Action where

import Monad
import SrcLoc
import Token

import qualified Data.Text as T

type Action = AlexInput -> Int -> Parser (Located Token)

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
token :: (T.Text -> Token) -> Action
token f (pos, _, _, inp) len = do
    let sp = mkSpan pos inp len
        t = T.take len inp
    return $ L sp (f t)

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
