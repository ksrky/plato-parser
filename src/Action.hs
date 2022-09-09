{-# LANGUAGE RankNTypes #-}

module Action where

import Error
import Monad
import SrcLoc
import Token

import Control.Monad.State
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
-- Layout rule
----------------------------------------------------------------
rightBrace :: Action
rightBrace (pos, _, _, inp) len = do
        let sp = mkSpan pos inp len
        il <- getIndentLevels
        case il of
                0 : _ -> return $ L sp (TokSymbol SymRBrace)
                _ -> lift $ throwPsError sp "missing an opening brace before closing"

leftBrace :: Action
leftBrace (pos, _, _, inp) len = do
        let sp = mkSpan pos inp len
        il <- getIndentLevels
        setIndentLevels (0 : il)
        return $ L sp (TokSymbol SymLBrace)
