module HW4.T2
    ( ParseError(..)
    , Parser(..)
    , runP
    , pChar
    , parseError
    , pEof
    , pAbbr
    , parseExpr
    ) where

import HW4.T1 (ExceptState(..), Except(..), mapExceptState, wrapExceptState, joinExceptState, modifyExceptState, throwExceptState)

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Data.Char (isUpper)
import Numeric.Natural (Natural)

data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
    deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P parser) input = runES parser (0, input)

pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ throwExceptState (ErrorAtPos 0)

instance Alternative Parser where
    empty = parseError
    (<|>) = mplus

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
    if null s
        then Success (() :# (pos, s))
        else Error (ErrorAtPos pos)

pAbbr :: Parser String
pAbbr = do
    abbr <- some (mfilter isUpper pChar)
    pEof
    pure abbr

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

parseExpr :: String -> Except ParseError Expr
parseExpr input = runP exprParser input
  where
    exprParser :: Parser Expr
    exprParser = pTerm `chainl` addOp

    addOp :: Parser (Expr -> Expr -> Expr)
    addOp = (char '+' *> pure OpAdd) <|> (char '-' *> pure OpSub)

    pTerm :: Parser Expr
    pTerm = pFactor `chainl` mulOp

    mulOp :: Parser (Expr -> Expr -> Expr)
    mulOp = (char '*' *> pure OpMul) <|> (char '/' *> pure OpDiv)

    pFactor :: Parser Expr
    pFactor = pNumber <|> pParen

    pNumber :: Parser Expr
    pNumber = do
        n <- read <$> some (satisfy isDigit)
        pure (Val (fromIntegral n))

    pParen :: Parser Expr
    pParen = char '(' *> exprParser <* char ')'

    chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
    chainl p op = do
        x <- p
        rest x
      where
        rest x = (do f <- op
                     y <- p
                     rest (f x y))
                 <|> pure x

    isDigit :: Char -> Bool
    isDigit c = c >= '0' && c <= '9'

    char :: Char -> Parser Char
    char c = mfilter (== c) pChar

    satisfy :: (Char -> Bool) -> Parser Char
    satisfy predicate = do
        c <- pChar
        if predicate c then pure c else empty