{-# LANGUAGE OverloadedStrings #-}

module Resolver where

import Error
import Fixity
import Name
import SrcLoc
import Syntax

import Control.Exception.Safe
import Control.Monad
import qualified Data.Map.Strict as M

data Tok = TExp (Located Expr) | TOp Op

linear :: MonadThrow m => OpDict -> Located Expr -> m [Tok]
linear opdict (L _ (OpExpr e1 lx@(L _ x) e2)) = do
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
        resolve :: MonadThrow m => OpDict -> Located a -> m (Located a)

instance Resolver Expr where
        resolve od (L sp exp) =
                L sp <$> case exp of
                        VarExpr{} -> return exp
                        AppExpr e1 e2 -> do
                                e1' <- resolve od e1
                                e2' <- resolve od e2
                                return $ AppExpr e1' e2'
                        e@OpExpr{} -> do
                                toks <- linear od (L sp e)
                                res <- parseNeg (Op (L NoSpan (varName "")) (-1) Nonfix) toks
                                case res of
                                        (L _ rese, []) -> return rese
                                        _ -> throwPsError sp "illegal infix expression"
                            where
                                parseNeg :: MonadThrow m => Op -> [Tok] -> m (Located Expr, [Tok])
                                parseNeg op1 (TExp e1 : rest) = parse op1 e1 rest
                                parseNeg _ (TOp _ : _) = error "unreachable: sequence of operator"
                                parseNeg _ [] = error "unreachable: empty toks"
                                parse :: MonadThrow m => Op -> Located Expr -> [Tok] -> m (Located Expr, [Tok])
                                parse _ e1 [] = return (e1, [])
                                parse op1@(Op _ prec1 fix1) e1 ((TOp op2@(Op lx prec2 fix2)) : rest)
                                        | prec1 == prec2 && (fix1 /= fix2 || fix1 == Nonfix) = throwPsError sp "illegal infix expression"
                                        | prec1 > prec2 || (prec1 == prec2 && fix1 == Leftfix) = return (e1, TOp op2 : rest)
                                        | otherwise = do
                                                (r, rest') <- parseNeg op2 rest
                                                let sp' = combineSpans (getSpan e1) (getSpan r)
                                                parse op1 (L sp' $ OpExpr e1 lx r) rest'
                                parse _ _ _ = error "unrechable: sequence of expression"
                        LamExpr xs e -> do
                                e' <- resolve od e
                                return $ LamExpr xs e'
                        LetExpr ds e -> do
                                e' <- resolve od e
                                return $ LetExpr ds e'
                        CaseExpr e alts -> do
                                e' <- resolve od e
                                alts' <- forM alts $ \(pi, ei) -> do
                                        ei' <- resolve od ei
                                        return (pi, ei')
                                return $ CaseExpr e' alts'
                        Factor e -> do
                                L _ e' <- resolve od e
                                return e'

instance Resolver Decl where
        resolve od (L sp d) =
                L sp <$> case d of
                        FuncDecl f args expr -> FuncDecl f args <$> resolve od expr
                        _ -> return d

instance Resolver TopDecl where
        resolve od (L sp td) =
                L sp <$> case td of
                        Decl d -> Decl <$> resolve od d
                        _ -> return td

resolveFixity :: MonadThrow m => OpDict -> Program -> m Program
resolveFixity od prg = do
        tds <- mapM (resolve od) (topDecls prg)
        return prg{topDecls = tds}
