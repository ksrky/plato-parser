module Layout where

import {-# SOURCE #-} Action
import Monad
import SrcLoc
import Token

layoutKeyword :: Keyword -> Action
spaces :: Action
layoutSpaces :: AlexInput -> Int -> Parser (Located Token)
rightBrace :: Action
leftBrace :: Action
pushSemicolon :: Action
popLayoutLevel :: Located Token -> Parser Span