{-# LANGUAGE LambdaCase #-}

module HW4.TypesUtils
  ( pChar
  , pEof
  , skipSpaces
  , skipSpacesAndDo
  , parseSign
  , parseWithPredicate
  , parseExpectedIgnoreResult
  , pAbbr
  ) where

import Control.Applicative
import Control.Monad
import Data.Char(isSpace, isUpper)
import HW4.Parser
import HW4.T1
import HW4.Types

pChar :: Parser Char
pChar =
  P (ES
       (\(pos, s) ->
          case s of
            []     -> Error (ErrorAtPos pos)
            (c:cs) -> Success (c :# (pos + 1, cs))))

pEof :: Parser ()
pEof =
  P (ES
       (\case
          (pos, "") -> Success (() :# (pos, ""))
          (pos, _) -> Error (ErrorAtPos pos)))

skipSpaces :: Parser ()
skipSpaces = do
  _ <- many (parseWithPredicate isSpace)
  return ()

skipSpacesAndDo :: Parser a -> Parser a
skipSpacesAndDo action = do
  skipSpaces
  action

parseSign :: [Char] -> Parser Char
parseSign signs = skipSpacesAndDo (parseWithPredicate $ combinePredicates signs)

parseWithPredicate :: (Char -> Bool) -> Parser Char
parseWithPredicate predicate = mfilter predicate pChar

parseExpectedIgnoreResult :: Char -> Parser ()
parseExpectedIgnoreResult expected = do
  _ <- parseWithPredicate (makePred expected)
  return ()

pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter isUpper pChar)
  pEof
  pure abbr

combinePredicates :: [Char] -> (Char -> Bool)
combinePredicates =
  Prelude.foldr (\char acc c -> makePred char c || acc c) (const False)

makePred :: Char -> (Char -> Bool)
makePred ch = (== ch)