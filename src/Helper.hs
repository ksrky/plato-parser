{-# LANGUAGE RankNTypes #-}

module Helper where

import Monad
import SrcLoc
import Token

import Control.Exception.Safe
import qualified Data.Text as T

type Parser a = ParserT (Either SomeException) a

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
        tok = T.take len inp
    return $ L sp (f tok)

keyword :: Keyword -> Action
keyword = token . const . TokKeyword

symbol :: Symbol -> Action
symbol = token . const . TokSymbol

varid :: Action
varid = token TokVarId

conid :: Action
conid = token TokConId

varsym :: Action
varsym = token TokVarSym

consym :: Action
consym = token TokConSym

int :: Action
int = token (TokInt . read . T.unpack)
