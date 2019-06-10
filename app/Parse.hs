{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Parse(parseDef) where

import Prelude hiding (exp, fail, lookup, pred)

import qualified Data.Char as Char

import EarleyM (Gram,Lang,fail,alts,fix,produce,declare,getToken,many,skipWhile)
import qualified EarleyM as EM(parse,Parsing(..))

import Ast(Exp,Def)
import qualified Ast(Exp(..),Def(..))

type Hopefully = Either String

parseDef :: String -> Hopefully (Either Def Exp)
parseDef s =
    case EM.parse lang s of
        EM.Parsing{EM.outcome} -> case outcome of
            Left pe -> Left $ show pe
            Right exp -> return exp

lang :: Lang Char (Gram (Either Def Exp))
lang = do

    token <- getToken

    let symbol x = do t <-token; if t==x then return () else fail
    let sat pred = do c <- token; if pred c then return c else fail

    let alpha = sat Char.isAlpha
    let numer = sat Char.isDigit
    let prime = sat (== '\'')
    let digit = do c <- numer; return (digitOfChar c)
    let white = do _ <- sat Char.isSpace; return ()

    digits <- fix"digits" $ \digits -> return $ alts [
        do n <- digits; d <- digit; return (10 * n + d),
        digit]

    let ident = do x <- alpha; xs <- many (alts [alpha,numer,prime]); return (x : xs)

    let ws = skipWhile white -- white*
    let ws1 = do white; ws -- white+
    let eps = return ()

    let underscore = do symbol '_'; return "_"
    let formal = alts [ident,underscore]

    let formals = parseListSep formal ws1

    let base = alts [fmap Ast.ENum digits,
                     fmap Ast.EVar ident]

    let parenthesized p = do symbol '('; ws; x <- p; ws; symbol ')'; return x

    let mkApp p1 sep p2 = do
            e1 <- p1
            sep::Gram()
            e2 <- p2;
            return $ Ast.EApp e1 e2

    let mkBin f c left right = do
            a <- left
            ws; symbol c; ws
            b <- right
            return (f a b)

    let mkLam exp = do
            symbol '\\'
            ws; xs <- formals
            ws; alts [symbol '.', do symbol '-'; symbol '>']
            ws;
            e <- exp
            return $ foldr Ast.ELam e xs

    (exp',exp) <- declare "exp"

    let par = parenthesized exp
    let lam = mkLam exp

    -- application: juxta position; but whitespace is required for base@base
    (app',app) <- declare "app"
    produce app' $ alts [
        mkApp (alts [base,par,app])    ws1     (alts [base,par]),
        mkApp (alts [base,par,app])    eps     (alts [     par]),
        mkApp (alts [     par    ])    eps     (alts [base    ])
        ]

    let app_lam = mkApp (alts [base,par,app]) ws lam

    -- left associative operators
    (opl',opl) <- declare "opl"
    produce opl' $ mkBin Ast.EAdd '+' (alts [base,par,app,opl]) (alts [base,par,app])
    let opl_lam  = mkBin Ast.EAdd '+' (alts [base,par,app,opl]) lam

    produce exp' $ alts [base,par,lam,app,opl, app_lam, opl_lam]

    let def = do
            name <- formal
            args <- many $ do ws1; formal
            ws; symbol '='
            ws; body <- exp
            return $ Ast.Def name (foldr Ast.ELam body args)

    let top = alts [
            do d <- def; return $ Left d,
            do e <- exp; return $ Right e]

    return $ do ws; x <- top; ws; return x

digitOfChar :: Char -> Int
digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

parseListSep :: Gram a -> Gram () -> Gram [a]
parseListSep p sep = alts [
    do x <- p; sep; xs <- parseListSep p sep; return (x : xs),
    do x <- p; return [x]]
