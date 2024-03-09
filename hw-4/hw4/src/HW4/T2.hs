{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}

module HW4.T2
  ( Parser(..)
  , ParseError(..)
  , runP
  , parseExpr
  ) where

import Control.Applicative
import HW4.ParserUtils
import HW4.Parser
import HW4.Types
import HW4.TypesUtils

parseExpr :: String -> Except ParseError Expr
parseExpr = runP expressionParser

expressionParser :: Parser Expr
expressionParser = do
  expr <- parseAddSub
  skipSpaces
  pEof
  return expr

parseAddSub :: Parser Expr
parseAddSub = parseBinOp ['+', '-'] parseMulDiv

parseMulDiv :: Parser Expr
parseMulDiv = parseBinOp ['*', '/'] parseTerm

parseBinOp :: [Char] -> Parser Expr -> Parser Expr
parseBinOp variants parseType = do
  parseRest variants parseType =<< parseType

parseRest :: [Char] -> Parser Expr -> Expr -> Parser Expr
parseRest variants parseType expr =
  skipSpacesAndDo
    (optional (parseSign variants)
       >>= (\case
              Just oper -> do
                term2 <- skipSpacesAndDo parseType
                let ex = Op (operationMap oper expr term2)
                parseRest [oper] parseType ex
              Nothing -> return expr))

parseTerm :: Parser Expr
parseTerm = parseValDouble <|> parseExprWithBrackets

parseExprWithBrackets :: Parser Expr
parseExprWithBrackets = do
  skipSpacesAndDo (parseExpectedIgnoreResult '(')
  term <- skipSpacesAndDo parseAddSub
  skipSpacesAndDo (parseExpectedIgnoreResult ')')
  return term

operationMap :: Char -> (a -> a -> Prim a)
operationMap =
  \case
    '+' -> Add
    '-' -> Sub
    '*' -> Mul
    _ -> Div

parseValDouble :: Parser Expr
parseValDouble = skipSpacesAndDo (Val <$> parseNumber)