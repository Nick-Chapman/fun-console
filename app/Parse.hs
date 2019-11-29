
module Parse(
  parseDef
  ) where

import Ast (Exp(..),Def(..),Base(..))
import Control.Monad(mfilter)
import EarleyM (Gram,Lang,fail,alts,fix,produce,declare,getToken,many,skipWhile)
import Prelude hiding (exp, fail, lookup, pred)
import qualified Ast
import qualified Data.Char as Char
import qualified EarleyM as EM(parse,Parsing(..))

type Hopefully = Either String

parseDef :: String -> Hopefully (Maybe (Either Def Exp))
parseDef s =
  case EM.parse lang s of
    EM.Parsing{EM.outcome} -> case outcome of
      Left pe -> Left $ show pe
      Right exp -> return exp

keywords :: [String] -- which are not allowed as identifiers
--keywords = ["let","in"]
keywords = []

lang :: Lang Char (Gram (Maybe (Either Def Exp)))
lang = do

    token <- getToken

    let symbol x = do t <-token; if t==x then return () else fail
    let sat pred = do c <- token; if pred c then return c else fail

    let eps = return ()
    let skip p = do _ <- p; eps
    let optional p = alts [p,eps]

    let alpha = sat Char.isAlpha
    let numer = sat Char.isDigit
    let prime = sat (== '\'')
    let digit = do c <- numer; return (digitOfChar c)
    let space = skip (sat Char.isSpace)

    digits <- fix"digits" $ \digits -> return $ alts [
        do n <- digits; d <- digit; return (10 * n + d),
        digit]

    let ident0 = do x <- alpha; xs <- many (alts [alpha,numer,prime]); return (x : xs)
    let ident = mfilter (`notElem` keywords) ident0

    let keyword string = mapM_ symbol string

    let ws = skipWhile space -- white*
    let ws1 = do space; ws -- white+

    let underscore = do symbol '_'; return "_"
    let formal = alts [ident,underscore]

    let formals = parseListSep formal ws1

    let num = fmap (EBase . BNum) digits
    let var = fmap EVar ident

    let dq = symbol '"'
    let notdq = sat (/= '"')
    let stringLit = do dq; cs <- many notdq; dq; return $ EBase $ BStr cs

    let parenthesized p = do symbol '('; ws; x <- p; ws; symbol ')'; return x

    let mkApp p1 sep p2 = do
            e1 <- p1
            sep::Gram()
            e2 <- p2;
            return $ EApp e1 e2

    let mkBin f c left right = do
            a <- left
            ws; keyword c; ws
            b <- right
            return (f a b)

    let makeBinop a b = alts [
            mkBin Ast.addition "+" a b,
            mkBin Ast.subtraction "-" a b,
            mkBin Ast.concatenation "^" a b,
            mkBin Ast.greater ">" a b,
            mkBin Ast.equalI "==" a b,
            mkBin Ast.equalS "===" a b
            ]

    let mkLam exp = do
            symbol '\\'
            ws; xs <- formals
            ws; alts [symbol '.', do symbol '-'; symbol '>']
            ws;
            e <- exp
            return $ foldr ELam e xs

    (exp',exp) <- declare "exp"

    let lam = mkLam exp

    let letSyntax = do
            keyword "let"
            ws; x <- formal
            ws; symbol '='
            ws; defined <- exp
            ws; keyword "in"
            ws; body <- exp
            return $ ELet x defined body

    let open = alts [num,var] -- requiring whitespace to avoid juxta-collision
    let closed = alts [parenthesized exp, stringLit]

    -- application: juxta position; but whitespace is required for open@open
    (app',app) <- declare "app"
    produce app' $ alts [
        mkApp (alts [open,closed,app])    ws1     (alts [open,closed]),
        mkApp (alts [open,closed,app])    eps     (alts [     closed]),
        mkApp (alts [     closed    ])    eps     (alts [open    ])
        ]

    let app_lam = mkApp (alts [open,closed,app]) ws lam

    -- left associative operators
    (opl',opl) <- declare "opl"
    produce opl' $ makeBinop (alts [open,closed,app,opl]) (alts [open,closed,app])
    let opl_lam  = makeBinop (alts [open,closed,app,opl]) lam

    produce exp' $ alts [open,closed,lam,app,opl, app_lam, opl_lam, letSyntax]

    let def = do
            name <- formal
            args <- many $ do ws1; formal
            ws; symbol '='
            ws; body <- exp
            return $ Def name (foldr ELam body args)


    let top = alts
          [ do d <- def; return $ Left d
          , do e <- exp; return $ Right e
          ]

    let dash = skip (sat (== '-'))
    let lineComment = do dash; dash; skipWhile (skip (sat (/= '\n')))

    return $ alts
      [ do eps; return Nothing
      , do lineComment; return Nothing
      , do ws1; optional lineComment; return Nothing
      , do ws; x <- top; ws; optional lineComment; return $ Just x
      ]

digitOfChar :: Char -> Int
digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

parseListSep :: Gram a -> Gram () -> Gram [a]
parseListSep p sep = alts [
    do x <- p; sep; xs <- parseListSep p sep; return (x : xs),
    do x <- p; return [x]]
