module Action where

import Monad
import SrcLoc
import Token

import Data.Text as T

type Action = AlexInput -> Int -> Parser (Located Token)

-- | Combinators
skip :: Action
andBegin :: Action -> Int -> Action
begin :: Int -> Action
ret :: Span -> Token -> Parser (Located Token)

-- | SrcLoc
mkLoc :: PsPosn -> Loc
mkSpan :: PsPosn -> T.Text -> Int -> Span

-- | Token
token :: (T.Text -> Token) -> Action
keyword :: Keyword -> Action
symbol :: Symbol -> Action
varid :: Action
conid :: Action
varsym :: Action
consym :: Action
qvarid :: Action
qconid :: Action
qvarsym :: Action
qconsym :: Action
integer :: Action

-- | Comment
beginComment :: Action
endComment :: Action
