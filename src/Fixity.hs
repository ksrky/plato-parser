module Fixity where

import Name
import SrcLoc

import qualified Data.Map.Strict as M

data Fixity = Leftfix | Rightfix | Nonfix
        deriving (Eq, Show)

data Op = Op (Located Name) Int Fixity
        deriving (Eq, Show)

type OpDict = M.Map Name Op

maxPrec :: Int
maxPrec = 9

minPrec :: Int
minPrec = 0