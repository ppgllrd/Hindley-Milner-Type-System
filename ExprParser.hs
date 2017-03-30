--------------------------------------------------------------------------------
-- A parser for expressions
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, 2017
--
--------------------------------------------------------------------------------

module ExprParser (parseExpr) where

import Data.Char(ord, isSpace)
import Lex
import Expr
import Mon
import Control.Monad
import Text.Parsec

type Parser a = Parsec String () a

p `sepBy2` sep = do
  x <- p
  xs <- many1 (do { sep; p })
  return (x:xs)

pSpaces = skipMany1 (satisfy isSpace)

pComment = do
  try (string "--")
  skipMany (satisfy (/= '\n'))

pJunk = do
  many (pSpaces <|> pComment)
  return ()

pToken p = do
  v <- try p
  pJunk
  return v

pSymbol str = pToken ((string str))

identifier ks = pToken $ do
  x <- ident
  if not (elem x ks) then return x
                     else mzero
  where
    ident = do
      x <- lower
      xs <- many alphaNum
      return (x:xs)

pBool =
    const (ConstB False) `fmap` pSymbol lxFalse
  <|>
    const (ConstB True) `fmap` pSymbol lxTrue

pInt = pToken $ ConstI `fmap` integer
  where
    nat = do { x <- digit; return (ord x - ord '0') } `chainl1` return op
    m `op` n = 10*m + n
    integer = do { char '-'; n <- nat; return (-n) } <|> nat

reservedWords = [lxIf, lxThen, lxElse, lxLet, lxLetrec, lxIn]
reservedCons  = [lxTrue, lxFalse, lxCons, lxNil]

pVarName = identifier reservedWords

pVar = Var `fmap` pVarName

                         -- ToDo: defined opt instead
pCons  = do vn <- pToken (do [x0] <- string lxUnder `mplus` return " "
                             x1 <- upper
                             xs <- many alphaNum
                             if [x0] /= lxUnder then return (x1:xs)
                                                else return (x0:x1:xs))
            if not (elem vn reservedCons) then return (Var vn)
                                          else mzero

pTuple = do
  { pSymbol lxOP
  ; do { pSymbol lxCP
       ; return (Tuple [])
       }
    <|>
    do { es <- pExpr `sepBy2` pSymbol lxComma
       ; pSymbol lxCP
       ; return (Tuple es)
       }
  }

pList = do
  { pSymbol lxOL
  ; es <- pExpr `sepBy` pSymbol lxComma
  ; pSymbol lxCL
  ; return (foldr cons nil es)
  }
  where
    cons x xs = App (App (Var lxCons) x) xs
    nil = Var lxNil

pAtom =
  choice [ pBool  -- Boolean value
         , pInt   -- Integer value
         , pVar   -- A variable
         , pCons  -- A constructor
         , pTuple -- A tuple
         , pList  -- A list
         ] <?> "atom"

pExpr = do
  es <- many1 pTerm
  return (foldl1 App es) -- Application of expressions

pTerm =
  choice [ pCond
         , pLambda
         , pLet
         , try pAtom -- needs try because lxOP can be a parethesized expr
         , between (pSymbol lxOP) (pSymbol lxCP) pExpr
         ] <?> "term"

pCond = do
  pSymbol lxIf
  e1 <- pExpr
  pSymbol lxThen
  e2 <- pExpr
  pSymbol lxElse
  e3 <- pExpr
  return (Cond e1 e2 e3)

pLambda = do
  pSymbol lxLambda
  vs <- many1 pVarName
  pSymbol lxArrow
  e <- pExpr
  return (foldr Abs e [ vn | vn <- vs ])

pLet = do
  pSymbol lxLet
  vn <- pVarName
  pSymbol lxEq
  e1 <- pExpr
  pSymbol lxIn
  e2 <- pExpr
  return (Let vn e1 e2)

parseExpr :: (Monad m) => String -> m Expr
parseExpr str = case runParser pExpr () "interactive" str of
                  Right expr -> return expr
                  Left error -> fail (show error)


testParser :: String -> Expr
testParser str = runMon undefined $ do
  parseExpr str
