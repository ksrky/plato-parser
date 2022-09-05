{-# LANGUAGE OverloadedStrings #-}

module Resolver where

import Fixity
import SrcLoc
import Syntax

import Control.Exception.Safe
import Control.Monad
import qualified Data.Map.Strict as M

data Tok = TExp Expr | TOp Op

linear :: MonadThrow m => OpDict -> Expr -> m [Tok]
linear opdict (OpExpr e1 lx@(L _ x) e2) = do
        e1' <- linear opdict e1
        e2' <- linear opdict e2
        let op = case M.lookup x opdict of
                Just op' -> op'
                Nothing -> Op lx maxPrec Leftfix
        return $ e1' ++ [TOp op] ++ e2'
linear opdict e = do
        e' <- resolve opdict e
        return [TExp e']

class Resolver a where
        resolve :: MonadThrow m => OpDict -> a -> m a

instance Resolver Expr where
        resolve od (AppExpr e1 e2) = do
                e1' <- resolve od e1
                e2' <- resolve od e2
                return $ AppExpr e1' e2'
        resolve od e@OpExpr{} = do
                toks <- linear od e
                fst <$> parseNeg (Op undefined (-1) Nonfix) toks
            where
                parseNeg :: MonadThrow m => Op -> [Tok] -> m (Expr, [Tok])
                parseNeg op1 (TExp e1 : rest) = parse op1 e1 rest
                parseNeg _ _ = undefined -- throwPsError ""
                parse :: MonadThrow m => Op -> Expr -> [Tok] -> m (Expr, [Tok])
                parse _ e1 [] = return (e1, [])
                parse op1@(Op _ prec1 fix1) e1 (TOp op2@(Op lx prec2 fix2) : rest)
                        | prec1 == prec2 && (fix1 /= fix2 || fix1 == Nonfix) = undefined --throwPsError
                        | prec1 > prec2 || (prec1 == prec2 && fix1 == Leftfix) = return (e1, TOp op2 : rest)
                        | otherwise = do
                                (r, rest') <- parseNeg op2 rest
                                parse op1 (OpExpr e1 lx r) rest'
                parse _ _ _ = undefined -- throwPsError
        resolve od (LamExpr xs e) = do
                e' <- resolve od e
                return $ LamExpr xs e'
        resolve od (LetExpr ds e) = do
                e' <- resolve od e
                return $ LetExpr ds e'
        resolve od (CaseExpr e alts) = do
                e' <- resolve od e
                alts' <- forM alts $ \(pi, ei) -> do
                        ei' <- resolve od ei
                        return (pi, ei')
                return $ CaseExpr e' alts'
        resolve _ e = return e

instance Resolver Decl where
        resolve od (FuncDecl f args expr) = FuncDecl f args <$> resolve od expr
        resolve _ d = return d

instance Resolver TopDecl where
        resolve od (Decl d) = Decl <$> resolve od d
        resolve _ td = return td

instance Resolver Program where
        resolve od prg = do
                tds <- mapM (resolve od) (topDecls prg)
                return prg{topDecls = tds}
