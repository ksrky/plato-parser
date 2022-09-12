module Lexer where

import Monad
import SrcLoc
import Token

code :: Int
layout :: Int
comment :: Int
alexMonadScan :: Parser (Located Token)
