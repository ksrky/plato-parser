{-# LANGUAGE DeriveTraversable #-}

--ref: https://github.com/ghc/ghc/blob/master/compiler/GHC/Types/SrcLoc.hs
module SrcLoc where

import Pretty

data Loc
        = Loc
                !Int -- line number
                !Int -- column number
        deriving (Eq, Ord, Show)

data Span = Span
        { spanStart :: Loc
        , spanEnd :: Loc
        }
        deriving (Eq, Ord, Show)

combineSpans :: Span -> Span -> Span
combineSpans (Span s1 e1) (Span s2 e2) = Span (s1 `min` s2) (e1 `max` e2)

data Located a = L Span a
        deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Pretty Loc where
        pretty (Loc l c) = show l ++ ":" ++ show c

instance Pretty Span where
        pretty (Span s e) = pretty s ++ "-" ++ pretty e
