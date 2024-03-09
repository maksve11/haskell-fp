module HW4.ParserUtils
  ( parseNumber
  ) where

import Control.Applicative (optional, some)
import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromMaybe)
import HW4.Parser
import HW4.TypesUtils

parseNumber :: Parser Double
parseNumber = do
  intPart <- parseInteger
  floatPart <- optional parseFloatinfPart
  return (fromRational (intPart + fromMaybe 0 floatPart))

parseFloatinfPart :: Parser Rational
parseFloatinfPart = do
  parseExpectedIgnoreResult '.'
  getFloatPart <$> parseDigits

parseInteger :: Parser Rational
parseInteger = toInt <$> parseDigits

getFloatPart :: [Char] -> Rational
getFloatPart chars = toInt chars / (10 ^ length chars)

toInt :: [Char] -> Rational
toInt = foldl (\acc ch -> acc * 10 + toRational (toInteger (digitToInt ch))) 0

parseDigits :: Parser String
parseDigits = some (parseWithPredicate isDigit)