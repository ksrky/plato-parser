{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

--ref: https://github.com/ghc/ghc/blob/master/compiler/GHC/Types/SrcLoc.hs
module SrcLoc where

import Pretty

data Loc
        = Loc
                !Int -- line number
                !Int -- column number
        deriving (Eq, Ord, Show)

instance Pretty Loc where
        pretty (Loc l c) = show l ++ ":" ++ show c

data Span = Span
        { spanStart :: Loc
        , spanEnd :: Loc
        }
        deriving (Eq, Ord, Show)

instance Pretty Span where
        pretty (Span s e) = pretty s ++ "-" ++ pretty e

combineSpans :: Span -> Span -> Span
combineSpans (Span s1 e1) (Span s2 e2) = Span (s1 `min` s2) (e1 `max` e2)

concatSpans :: [Span] -> Span
concatSpans [] = undefined
concatSpans [sp] = sp
concatSpans (sp : sps) = combineSpans sp (concatSpans sps)

data Located a = L Span a
        deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

unitLoc :: Span -> Located ()
unitLoc sp = L sp ()

getSpan :: Located a -> Span
getSpan (L sp _) = sp
