module Token where

import Pretty

import Data.Text

data Token
        = TokKeyword Keyword
        | TokSymbol Symbol
        | TokVarId Text
        | TokQVarId Text
        | TokConId Text
        | TokQConId Text
        | TokVarSym Text
        | TokQVarSym Text
        | TokConSym Text
        | TokQConSym Text
        | TokInt Int
        | TokEOF
        deriving (Eq, Show)

data Keyword
        = KwCase
        | KwData
        | KwForall
        | KwImport
        | KwInfix
        | KwInfixL
        | KwInfixR
        | KwIn
        | KwOf
        | KwLet
        | KwModule
        | KwWhere
        deriving (Eq, Show)

data Symbol
        = SymArrow
        | SymAt
        | SymBackslash
        | SymColon
        | SymComma
        | SymDash
        | SymDot
        | SymEqual
        | SymLBrace
        | SymLBrack
        | SymLParen
        | SymRBrace
        | SymRBrack
        | SymRParen
        | SymSemicolon
        | SymUScore
        | SymVBar
        deriving (Eq, Show)

instance Pretty Token where
        pretty (TokKeyword k) = pretty k
        pretty (TokSymbol t) = pretty t
        pretty (TokVarId t) = show t
        pretty (TokQVarId t) = show t
        pretty (TokConId t) = show t
        pretty (TokQConId t) = show t
        pretty (TokVarSym t) = show t
        pretty (TokQVarSym t) = show t
        pretty (TokConSym t) = show t
        pretty (TokQConSym t) = show t
        pretty (TokInt n) = show n
        pretty TokEOF = "<eof>"

instance Pretty Keyword where
        pretty KwCase = "case"
        pretty KwData = "data"
        pretty KwForall = "forall"
        pretty KwImport = "import"
        pretty KwInfix = "infix"
        pretty KwInfixL = "infixl"
        pretty KwInfixR = "infixr"
        pretty KwIn = "in"
        pretty KwOf = "of"
        pretty KwLet = "let"
        pretty KwModule = "module"
        pretty KwWhere = "where"

instance Pretty Symbol where
        pretty SymArrow = "->"
        pretty SymAt = "@"
        pretty SymBackslash = "\\"
        pretty SymColon = ":"
        pretty SymComma = ","
        pretty SymDash = "'"
        pretty SymDot = "."
        pretty SymEqual = "="
        pretty SymLBrace = "{"
        pretty SymLBrack = "["
        pretty SymLParen = "("
        pretty SymRBrace = "{"
        pretty SymRBrack = "]"
        pretty SymRParen = ")"
        pretty SymSemicolon = ";"
        pretty SymUScore = "_"
        pretty SymVBar = "|"